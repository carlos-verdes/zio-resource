package io.funkode.resource
package outbound

import io.funkode.arangodb.*
import io.funkode.arangodb.http.*
import io.funkode.arangodb.model.*
import io.funkode.arangodb.http.JsonCodecs.given
import io.lemonlabs.uri.{Url, Urn}
import zio.*
import zio.json.*
import zio.json.ast.Json
import zio.http.*
import zio.test.*
import adapter.ArangoResourceStore
import io.funkode.portfolio
import io.funkode.portfolio.model.*
import io.funkode.resource.model.*
import io.funkode.resource.model.given
import io.funkode.resource.model.Resource.*
import zio.stream.{ZPipeline, ZStream}

trait TransactionsExamples:

  import Portfolio.given

  val hash1 = "0x888333"
  val timestamp1 = 1L

  val ethNetworkUrn = Urn.parse("urn:network:eth")
  val tx1Urn = Urn.parse("urn:tx:" + hash1 + "@" + ethNetworkUrn.nss)

  val ethNetwork = Network("eth", "1", "Ethereum Mainnet", "ETH")
  val ethNetwornJsonString =
    """
      |{
      |  "id": "eth",
      |  "chainId": "1",
      |  "name": "Ethereum Mainnet",
      |  "currency": "ETH"
      |}
      |""".stripMargin

  val ethNetworkResource = Resource.apply(ethNetworkUrn, ZStream.fromIterable(ethNetwornJsonString.getBytes))

  val tx1JsonString =
    s"""
       |{
       |  "network": $ethNetwornJsonString,
       |  "hash": "0x888333",
       |  "timestamp": 1
       |}
       |""".stripMargin

  val tx1 = io.funkode.portfolio.model.Transaction(ethNetwork, hash1, timestamp1)
  val tx1Resource = Resource.apply(tx1Urn, ZStream.fromIterable(tx1JsonString.getBytes()))

object ArangoStoreIT extends ZIOSpecDefault with TransactionsExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Store transaction") {
      for
        storedNetworkResource <- ResourceStore.store(ethNetworkResource)
        storedNetwork <- storedNetworkResource.of[Network].body
        fetchedNetworkResource <- ResourceStore.fetch(ethNetworkUrn)
        fetchedNetwork <- fetchedNetworkResource.of[Network].body
        storedTxResource <- ResourceStore.store(tx1Resource)
        storedTx <- storedTxResource.of[io.funkode.portfolio.model.Transaction].body
        fetchedTxResource <- ResourceStore.fetch(tx1Urn)
        fetchedTx <- fetchedTxResource.of[io.funkode.portfolio.model.Transaction].body
      yield assertTrue(storedNetwork == ethNetwork) &&
        assertTrue(storedNetwork == fetchedNetwork) &&
        assertTrue(storedTx == tx1) &&
        assertTrue(storedTx == fetchedTx)
    }).provideShared(
      Scope.default,
      ArangoConfiguration.default,
      Client.default,
      ArangoClientJson.testContainers,
      ArangoResourceStore.derived[Portfolio]
    )
