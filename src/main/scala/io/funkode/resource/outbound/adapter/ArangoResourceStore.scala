/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package outbound
package adapter

import scala.deriving.Mirror

import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.stream.*

import io.funkode.arangodb.http.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.funkode.arangodb.model.*
import io.funkode.resource.model.*
import io.funkode.velocypack.VPack.*

class ArangoResourceStore(db: ArangoDatabaseJson) extends ResourceStore:

  import ArangoResourceStore.*
  import ArangoResourceStore.given
  implicit val jsonCodec: JsonCodec[Json] = JsonCodec[Json](Json.encoder, Json.decoder)

  case class Rel(_rel: String, _from: DocumentHandle, _to: DocumentHandle, _key: DocumentKey)
      derives JsonCodec

  private def relCollection(urn: Urn): CollectionName = CollectionName(urn.nid + "-rels")

  private def linkKey(rel: String, rightUrn: Urn) =
    DocumentKey(s"""${rel}:${rightUrn.nid}:${rightUrn.nss}""")

  def fetch(urn: Urn): ResourceApiCall[Resource] =
    db
      .document(urn)
      .readRaw()
      .handleErrors(Some(urn))
      .map(stream => Resource.apply(urn, stream))

  def store(resource: Resource): ResourceApiCall[Resource] =
    resource.format match
      case ResourceFormat.Json =>
        val urn = resource.id

        for
          jsonDocument <- JsonDecoder[Json].decodeJsonStreamInput(resource.body).catchAll {
            case t: Throwable =>
              ZIO.fail(ResourceError.SerializationError("Error reading json resource body", Some(t)))
          }
          vobject <- jsonDocument match
            case jsonObj: Json.Obj =>
              ZIO.succeed(JsonCodecs.jsonObjectToVObject(jsonObj))
            case other =>
              ZIO.fail(ResourceError.FormatError(s"only supported to store json objects, received $other"))
          savedResource <- db
            .document(urn)
            .upsert(vobject)
            .handleErrors(Some(urn))
            .flatMap(vpack =>
              ZIO
                .fromEither[String, Json](vobjectEncoder.toJsonAST(vpack))
                .catchAll(encodeError => ZIO.fail(ResourceError.SerializationError(encodeError)))
            )
            .map(_.asResource)
        yield savedResource

  def link(leftUrn: Urn, rel: String, rightUrn: Urn): ResourceApiCall[Unit] =
    db.collection(relCollection(leftUrn))
      .documents
      .create(List(Rel(rel, leftUrn, rightUrn, linkKey(rel, rightUrn))))
      .handleErrors()
      .map(_ => ())

  def fetchRel(urn: Urn, relType: String): ResourceStream[Resource] =
    db
      .query(
        Query("FOR v, e IN OUTBOUND @startVertex @@edge FILTER e._rel == @relType RETURN v")
          .bindVar("startVertex", VString(fromUrnToDocHandle(urn).unwrap))
          .bindVar("@edge", VString(relCollection(urn).unwrap))
          .bindVar("relType", VString(relType))
      )
      .stream[Json]
      .map(json => json.asResource)
      .handleStreamErrors()

object ArangoResourceStore:

  import ArangoError.*
  import ResourceError.*

  val RelsCollection = CollectionName("rels")

  val InternalKeys = Seq(VObject.IdKey, VObject.KeyKey, VObject.RevKey)

  given fromUrnToDocHandle: Conversion[Urn, DocumentHandle] = urn =>
    DocumentHandle(CollectionName(urn.nid), DocumentKey(urn.nss))

  given fromDocHandleToUrn: Conversion[DocumentHandle, Urn] = docHandle =>
    Urn.parse(s"urn:${docHandle.collection.unwrap}:${docHandle.key.unwrap}")

  def handleArrangoErrors(urn: Option[Urn], t: Throwable): ResourceError = t match
    case e @ ArangoError(404, _, message, _) => ResourceError.NotFoundError(urn, Some(e))
    case e                                   => ResourceError.UnderlinedError(e)

  extension [R](arangoIO: IO[ArangoError, R])
    def handleErrors(urn: Option[Urn] = None): ResourceApiCall[R] =
      arangoIO.catchAll(t => ZIO.fail(handleArrangoErrors(urn, t)))

  extension [R](arangoStream: Stream[ArangoError, R])
    def handleStreamErrors(urn: Option[Urn] = None): ResourceStream[R] =
      arangoStream.catchAll(t => ZStream.fail(handleArrangoErrors(urn, t)))

  extension (json: Json)
    def etag: Option[Etag] = json match
      case Json.Obj(fields) =>
        fields.filter(_._1 == VObject.RevKey).map(_._2.as[String].toOption.map(Etag.apply)).headOption.flatten
      case _ =>
        None

    def documentHandle: DocumentHandle = json match
      case Json.Obj(fields) =>
        fields
          .filter(_._1 == VObject.IdKey)
          .map(_._2.as[String].toOption.map(DocumentHandle.parse).flatten)
          .headOption
          .flatten
          .get // risky option but ArangoDB always retrieves _id
      case other => throw new Exception(s"not expected an ArangoDB document without _id, received: $other")

    def pure: Json = json match
      case Json.Obj(fields) => Json.Obj(fields.filterNot(t => InternalKeys.contains(t._1)))
      case other            => other

    def asResource: Resource =
      val urn: Urn = fromDocHandleToUrn(json.documentHandle)
      val body: ByteResourceStream = ZStream.fromIterable(json.pure.toJson.toCharArray.map(_.toByte))
      val etag: Option[Etag] = json.etag

      Resource(urn, body, ResourceFormat.Json, etag)

  def initDb(arango: ArangoClientJson, resourceModel: ResourceModel): ResourceApiCall[ArangoDatabaseJson] =
    val db = arango.database(DatabaseName(resourceModel.name))

    (db.createIfNotExist() *>
      ZIO
        .collectAll {
          val createCollections = resourceModel.collections
            .map(_._1)
            .map(CollectionName.apply)
            .map(col => db.collection(col).createIfNotExist())

          val createRels = resourceModel.collections
            .map(c => CollectionName(c._1 + "-rels"))
            .map(col => db.collection(col).createEdgeIfNotExist())

          createCollections ++ createRels
        }).handleErrors() *>
      ZIO.succeed(db)

  inline def derived[R: Mirror.Of]: ZLayer[ArangoClientJson, ResourceError, ResourceStore] =
    ZLayer(
      for
        client <- ZIO.service[ArangoClientJson]
        resourceModel = DeriveResourceModel.gen[R]
        db <- initDb(client, resourceModel)
      yield new ArangoResourceStore(db)
    )
