/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import scala.quoted.{Expr, Quotes, Type}

import cats.Show
import cats.syntax.show.toShow
import io.lemonlabs.uri.Urn
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.schema.meta.MetaSchema
import zio.stream.*

type ResourceStream[R] = Stream[ResourceError, R]
type ByteResourceStream = ResourceStream[Byte]

enum ResourceFormat:
  case Json

opaque type Etag = String
object Etag:
  def apply(etag: String): Etag = etag
  extension (etag: Etag) def unwrap: String = etag

enum ResourceError(msg: String, cause: Option[Throwable] = None) extends Throwable(msg, cause.orNull):
  case NotFoundError(urn: Option[Urn])
      extends ResourceError(s"""Resource ${urn.map("with id " + _).getOrElse("(with no urn)")} not found""")
  case SerializationError(msg: String, cause: Option[Throwable] = None) extends ResourceError(msg, cause)
  case FormatError(msg: String, cause: Option[Throwable] = None)
      extends ResourceError(s"Format not supported: $msg", cause)
  case UnderlinedError(cause: Throwable) extends ResourceError("Non controlled error", Some(cause))

case class ResourceLink(urn: Urn, rel: String, attributes: Map[String, String] = Map.empty)
type ResourceLinks = Map[String, ResourceLink]

trait Resource:

  def id: Urn

  def body: ByteResourceStream

  def format: ResourceFormat = ResourceFormat.Json

  def etag: Option[Etag] = None

  def links: ResourceLinks = Map.empty

  override def equals(thatAny: Any): Boolean =
    if thatAny.isInstanceOf[Resource] then
      val that = thatAny.asInstanceOf[Resource]
      this.id == that.id &&
      this.format == that.format &&
      ((this.etag, that.etag) match
        case (Some(thisEtag), Some(thatEtag)) => thisEtag == thatEtag
        case _                                => true
      )
    else false

object Resource:

  given Show[Resource] = new Show[Resource]:
    def show(r: Resource): String =
      s"""Resource(${r.id}, ${r.format}${r.etag
          .map(e => ", etag: \"" + e + "\"")
          .getOrElse("")})"""

  /*
  trait Of[R] extends Resource, Identifiable[R]:

    def innerInstance: R

    override def id: Urn = Urn.parse(s"urn:$resourceNid:${resourceNss(innerInstance)}")

    def resourceNid: String
    def resourceNss(r: R): String
    def deserialize(): IO[ResourceError, R]
   */

  trait Identifiable[R]:
    self =>

    def resourceNid: String
    def resourceNss(r: R): String
    def resourceUrn(r: R): Urn = Urn.parse(s"urn:$resourceNid:${resourceNss(r)}")
    def resourceWithNss(r: R)(newNss: String): R

    extension (r: R)
      def nid: String = self.resourceNid
      def nss: String = resourceNss(r)
      def urn: Urn = resourceUrn(r)
      def withNss(nss: String): R = resourceWithNss(r)(nss)
