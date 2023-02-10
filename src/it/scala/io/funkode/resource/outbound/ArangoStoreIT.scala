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
  val ethNetworkResource = new Resource:
    def id: Urn = ethNetworkUrn

    def body: ByteResourceStream = ZStream.fromIterable("""
        |{
        |  "id": "eth",
        |  "chainId": "1",
        |  "name": "Ethereum Mainnet",
        |  "currency": "ETH"
        |}
        |""".stripMargin.toCharArray.map(_.toByte))

  val tx1 = io.funkode.portfolio.model.Transaction(ethNetwork, hash1, timestamp1)
  val tx1Resource = new Resource:
    def id: Urn = tx1Urn

    def body: ByteResourceStream = ZStream.fromIterable("""
        |{
        |  "network": "urn:network:eth",
        |  "hash": "0x888333",
        |  "timestamp": 1
        |}
        |""".stripMargin.toCharArray.map(_.toByte))

object ArangoStoreIT extends ZIOSpecDefault with TransactionsExamples:

  override def spec: Spec[TestEnvironment, Any] =
    suite("Arango ResourceStore should")(test("Store transaction") {
      for
        storedNetwork <- ResourceStore.store(ethNetworkResource)
        _ <- storedNetwork.body.via(ZPipeline.utf8Decode).runHead.debug("storedNetwork: ")
        fetchedNetwork <- ResourceStore.fetch(ethNetworkUrn)
        _ <- fetchedNetwork.body.via(ZPipeline.utf8Decode).runHead.debug("fetchedNetwork: ")
        storedTx <- ResourceStore.store(tx1Resource)
        fetchedTx <- ResourceStore.fetch(tx1Urn)
      yield assertTrue(storedTx == tx1Resource) &&
        assertTrue(fetchedTx == tx1Resource) &&
        assertTrue(storedNetwork == ethNetworkResource) &&
        assertTrue(fetchedNetwork == ethNetworkResource)
    }).provideShared(
      Scope.default,
      ArangoConfiguration.default,
      Client.default,
      ArangoClientJson.live,
      ArangoResourceStore.derived[Portfolio]
    )
