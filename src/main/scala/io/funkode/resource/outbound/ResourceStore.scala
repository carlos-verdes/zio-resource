/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource
package outbound

import scala.compiletime.*
import scala.quoted.*

import io.lemonlabs.uri.Urn
import io.netty.util.internal.StringUtil
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.schema.*
import zio.schema.meta.MetaSchema
import zio.stream.*
import io.funkode.resource.model.*
import io.funkode.resource.model.Resource.Addressable

type ResourceApiCall[R] = IO[ResourceError, R]

trait ResourceStore:

  def resourceModel: ResourceModel

  def fetch(urn: Urn): ResourceApiCall[Resource]
  def save(resource: Resource): ResourceApiCall[Resource]
  def link(leftUrn: Urn, relType: String, rightUrn: Urn): ResourceApiCall[Unit]
  def fetchRel(urn: Urn, relType: String): ResourceStream[Resource]

  inline def fetchAs[R: Resource.Addressable](urn: Urn): ResourceApiCall[Resource.Of[R]] =
    fetch(urn).map(_.of[R])

  inline def save[R: Resource.Addressable](
      inline addressable: R
  ): ResourceApiCall[Resource.Of[R]] =
    save(addressable.asJsonResource).map(_.of[R])

  inline def save[R: Resource.Addressable](
      inline typedResource: Resource.Of[R]
  ): ResourceApiCall[Resource] =
    save(typedResource.asJsonResource)

object ResourceStore:

  type WithResourceStore[R] = ZIO[ResourceStore, ResourceError, R]
  type WithResourceStreamStore[R] = ZStream[ResourceStore, ResourceError, R]

  inline def withStore[R](f: ResourceStore => WithResourceStore[R]) = ZIO.service[ResourceStore].flatMap(f)

  def fetch(urn: Urn): WithResourceStore[Resource] = withStore(_.fetch(urn))

  inline def fetchAs[R: Addressable](urn: Urn): WithResourceStore[Resource.Of[R]] = withStore(
    _.fetchAs[R](urn)
  )

  def save(resource: Resource): WithResourceStore[Resource] =
    withStore(_.save(resource))

  inline def save[R: Resource.Addressable](
      inline addressable: R
  ): WithResourceStore[Resource.Of[R]] =
    withStore(_.save(addressable))

  inline def save[R: Resource.Addressable](
      inline typedResource: Resource.Of[R]
  ): WithResourceStore[Resource] =
    withStore(_.save(typedResource))

  def link(leftUrn: Urn, relType: String, rightUrn: Urn): WithResourceStore[Unit] =
    withStore(_.link(leftUrn, relType, rightUrn))

  def fetchRel(urn: Urn, relType: String): WithResourceStreamStore[Resource] =
    ZStream.service[ResourceStore].flatMap(_.fetchRel(urn, relType))

/*
trait JsonStore extends ResourceStore[JsonEncoder, JsonDecoder, Json]:
  override type DocResource = JsonResource

object JsonStore:

  type WithJsonStore[R] = ZIO[JsonStore, ResourceError, R]

  def withStore[R](f: JsonStore => WithJsonStore[R]) = ZIO.service[JsonStore].flatMap(f)

  def fetch(urn: Urn): WithJsonStore[JsonResource] = withStore(_.fetch(urn))

  def store[R: JsonEncoder](urn: Urn, r: R): WithJsonStore[JsonResource] =
    withStore(_.store(urn, r))

  def store[R: JsonEncoder: Identifiable](r: R): WithJsonStore[JsonResource] = store(r.urn, r)

  extension (resourceIO: WithJsonStore[JsonResource])
    def deserialize[R: JsonDecoder] = resourceIO.flatMap(_.deserialize[R])
 */
