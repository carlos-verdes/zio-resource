/*
 * Copyright 2023 Carlos Verdes
 *
 * SPDX-License-Identifier: MIT
 */

package io.funkode.resource.model

import io.lemonlabs.uri.Urn
import zio.ZIO
import zio.json.JsonCodec
import zio.stream.ZStream
import zio.test.*

import io.funkode.portfolio.model.Portfolio
import io.funkode.resource.model.StoreModelDerivationSpec.{suite, test}

trait ResourceExamples:
  sealed trait Library

  case class Catalog(id: String, books: List[Book]) extends Library
  case class Author(id: String, name: String, age: Int) extends Library derives JsonCodec
  case class Book(isbn: String, title: String, author: Option[Author]) extends Library

  object Library:

    given Resource.Typed[Catalog] with
      def resourceCollection: String = "catalog"
      def resourceId(catalog: Catalog): String = catalog.id
      def resourceWithId(catalog: Catalog)(newId: String): Catalog = catalog.copy(id = newId)

    given Resource.Typed[Author] with
      def resourceCollection: String = "author"
      def resourceId(author: Author): String = author.name
      def resourceWithId(author: Author)(newId: String): Author = author.copy(id = newId)

    given Resource.Typed[Book] with
      def resourceCollection: String = "book"
      def resourceId(book: Book): String = book.isbn
      def resourceWithId(book: Book)(newId: String): Book = book.copy(isbn = newId)

  val jsonResourceUrn = Urn.parse("urn:user:peter")
  val jsonResourceBody = """
      |{
      |  "id": "123",
      |  "name": "Peter",
      |  "age": 23
      |}
      |""".stripMargin
  val jsonResource: Resource = Resource.fromString(jsonResourceUrn, jsonResourceBody)

  val personResource: Resource.Of[Author] =
    Resource.fromCaseClass(jsonResourceUrn, Author("123", "Peter", 23))

  val catalogUrn = Urn.parse("urn:catalog:mainCatalog")
  val authorUrn = Urn.parse("urn:author:miguel-cervantes")
  val bookUrn = Urn.parse("urn:book:9780744525021")

  val catalogJson: String =
    s"""
       |{
       |  "id": "mainCatalog",
       |  "books": [
       |    {
       |      "isbn": "9780744525021",
       |      "title": "El Quijote",
       |      "author": {
       |        "id": "miguel-cervantes",
       |        "name": "Miguel Cervantes",
       |        "age": 57
       |      }
       |    }
       |  ]
       |}
       |""".stripMargin

  // val denormalizedCatalogResource = Resource.fromString(catalogUrn, catalogJson)

  val normalizedCatalogJson = """{ "id": "mainCatalog" }""".stripMargin
  val normalizedBookJson = """{ "isbn": "9780744525021", "title": "El Quijote" }""".stripMargin
  val normalizedAuthorJson =
    """{ "id": "miguel-cervantes", "name": "Miguel Cervantes", "age": 57 }""".stripMargin

  val catalogResourceDenormalized =
    ZStream[Resource](
      Resource.fromString(catalogUrn, normalizedCatalogJson),
      Resource.fromString(bookUrn, normalizedBookJson),
      Resource.fromString(authorUrn, normalizedAuthorJson)
    )

object ResourceOfDerivationSpec extends ZIOSpecDefault with ResourceExamples:

  import Resource.fromRawResourceToTypedResource

  override def spec: Spec[TestEnvironment, Any] =
    suite("Resource should")(
      test("Create a domain resource from json with resource.of[R]") {
        val parsedResource = jsonResource.of[Author]
        for
          parsedBody <- parsedResource.body
          personBody <- personResource.body
        yield assertTrue(parsedResource.id == personResource.id) && assertTrue(parsedBody == personBody)
      },
      test("Denormalize a resource (document) with resource.denormalize") {
        val parsedResource = jsonResource.of[Author]
        for
          parsedBody <- parsedResource.body
          personBody <- personResource.body
        yield assertTrue(parsedResource.id == personResource.id) && assertTrue(parsedBody == personBody)
      }
    )
