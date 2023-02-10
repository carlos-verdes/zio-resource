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
import zio.schema.*
import zio.schema.codec.JsonCodec.*
import zio.stream.*

import io.funkode.arangodb.http.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.funkode.arangodb.model.*
import io.funkode.resource.model.*
import io.funkode.velocypack.VPack.VObject

class ArangoResourceStore(db: ArangoDatabaseJson) extends ResourceStore:

  import ArangoResourceStore.*
  import ArangoResourceStore.given

  def fetch(urn: Urn): ResourceApiCall[Resource] =
    db
      .document(urn)
      .read[Json]()
      .handleErrors(Some(urn))
      .map(_.asResource)

  def store(resource: Resource): ResourceApiCall[Resource] =

    val urn = resource.id
    val docStringStream = resource.body.via(ZPipeline.utf8Decode)

    docStringStream.tap(doc => Console.printLine(doc))

    for
      docStringOp <- docStringStream.runHead.catchAll { case t: Throwable =>
        ZIO.fail(ResourceError.SerializationError("Error procesing resource stream", Some(t)))
      }
      json <- docStringOp match
        case Some(docString) =>
          ZIO.fromEither(docString.fromJson[Json]).catchAll { case error: String =>
            ZIO.fail(ResourceError.SerializationError("Error deserializing resource to json: " + error))
          }
        case None => ZIO.fail(ResourceError.FormatError("Error storing resource, found empty"))
      result <- json match
        case jsonObj: Json.Obj =>
          db
            .document(urn)
            .upsert(JsonCodecs.jsonObjectToVObject(jsonObj))
            .handleErrors(Some(urn))
            .flatMap(vpack =>
              ZIO
                .fromEither[String, Json](vobjectEncoder.toJsonAST(vpack))
                .catchAll(encodeError => ZIO.fail(ResourceError.SerializationError(encodeError)))
            )
            .map(_.asResource)

        case other =>
          ZIO.fail(ResourceError.FormatError(s"only supported to store json objects, received $other"))
    yield result

  def link(leftUrn: Urn, relType: String, rightUrn: Urn): ResourceApiCall[Unit] = ???

  def fetchRel(urn: Urn, relType: String): ResourceStream[Resource] = ???

/*
  def store[R: JsonEncoder](urn: Urn, r: R): ResourceApiCall[Resource] =
    ZIO
      .fromEither(r.toJsonAST)
      .catchAll(encodeError => ZIO.fail(ResourceError.SerializationError(encodeError)))
      .flatMap(json => store(urn, json))
 */

object ArangoResourceStore:

  import ArangoError.*
  import ResourceError.*

  val InternalKeys = Seq(VObject.IdKey, VObject.KeyKey, VObject.RevKey)

  given fromUrnToDocHandle: Conversion[Urn, DocumentHandle] = urn =>
    DocumentHandle(CollectionName(urn.nid), DocumentKey(urn.nss))

  given fromDocHandleToUrn: Conversion[DocumentHandle, Urn] = docHandle =>
    Urn.parse(s"urn:${docHandle.collection.unwrap}:${docHandle.key.unwrap}")

  extension [R](arangoIO: IO[ArangoError, R])
    def handleErrors(urn: Option[Urn] = None): ResourceApiCall[R] = arangoIO.catchAll {

      case ArangoError(404, _, _, _) => ZIO.fail(ResourceError.NotFoundError(urn))
      case e                         => ZIO.fail(ResourceError.UnderlinedError(e))
    }

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
      new Resource:
        def id: Urn = fromDocHandleToUrn(json.documentHandle)

        def body: ByteResourceStream = ZStream.fromIterable(json.pure.toJson.toCharArray.map(_.toByte))

        override def etag: Option[Etag] = json.etag

        def deserialize[R: JsonDecoder]: IO[ResourceError, R] = ZIO
          .fromEither(json.as[R])
          .catchAll(decodeError => ZIO.fail(ResourceError.SerializationError(decodeError)))

  def initDb(arango: ArangoClientJson, resourceModel: ResourceModel): ResourceApiCall[ArangoDatabaseJson] =
    val db = arango.database(DatabaseName(resourceModel.name))

    db.createIfNotExist().handleErrors() *>
      ZIO
        .collectAll {
          resourceModel.collections
            .map(_._1)
            .map(CollectionName.apply)
            .map(col => db.collection(col).createIfNotExist())
        }
        .handleErrors() *>
      ZIO.succeed(db)

  inline def derived[R: Mirror.Of]: ZLayer[ArangoClientJson, ResourceError, ResourceStore] =
    ZLayer(
      for
        client <- ZIO.service[ArangoClientJson]
        resourceModel = DeriveResourceModel.gen[R]
        db <- initDb(client, resourceModel)
      yield new ArangoResourceStore(db)
    )
