package controllers

import java.util.UUID

import com.github.tomakehurst.wiremock.WireMockServer
import com.github.tomakehurst.wiremock.client.WireMock.{okJson, post, serverError, urlEqualTo}
import configuration.GraphQLConfiguration
import errors.AuthorisationException
import graphql.codegen.GetConsignment.{getConsignment => gc}
import graphql.codegen.UpdateTransferInitiated.{updateTransferInitiated => ut}
import io.circe.Printer
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.Matchers._
import org.scalatest.concurrent.ScalaFutures._
import play.api.Configuration
import play.api.Play.materializer
import play.api.i18n.Langs
import play.api.mvc.Result
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, contentAsString, contentType, redirectLocation, status => playStatus, _}
import play.api.test.WsTestClient.InternalWSClient
import services.ConsignmentExportService
import uk.gov.nationalarchives.tdr.GraphQLClient
import uk.gov.nationalarchives.tdr.GraphQLClient.Extensions
import util.{EnglishLang, FrontEndTestHelper}

import scala.concurrent.ExecutionContext
import scala.jdk.CollectionConverters.ListHasAsScala

class TransferSummaryControllerSpec extends FrontEndTestHelper {
  implicit val ec: ExecutionContext = ExecutionContext.global

  val wiremockServer = new WireMockServer(9006)
  val wiremockExportServer = new WireMockServer(9007)

  override def beforeEach(): Unit = {
    wiremockServer.start()
    wiremockExportServer.start()
  }

  override def afterEach(): Unit = {
    wiremockServer.resetAll()
    wiremockExportServer.resetAll()
    wiremockServer.stop()
    wiremockExportServer.stop()
  }

  val langs: Langs = new EnglishLang
  val consignmentId = UUID.randomUUID()

  def exportService(configuration: Configuration): ConsignmentExportService = {
    val wsClient = new InternalWSClient("http", 9007)
    new ConsignmentExportService(wsClient, configuration, new GraphQLConfiguration(configuration))
  }

  "TransferSnummaryController GET" should {

    "render the transfer summary page with an authenticated user" in {

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents,
        new GraphQLConfiguration(app.configuration), getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val client = new GraphQLConfiguration(app.configuration).getClient[gc.Data, gc.Variables]()
      val consignmentResponse: gc.GetConsignment = new gc.GetConsignment(UUID.randomUUID(), UUID.randomUUID())
      val data: client.GraphqlData = client.GraphqlData(Some(gc.Data(Some(consignmentResponse))), List())
      val dataString: String = data.asJson.printWith(Printer(dropNullValues = false, ""))
      wiremockServer.stubFor(post(urlEqualTo("/graphql"))
        .willReturn(okJson(dataString)))

      val transferSummaryPage = controller.transferSummary(consignmentId)
        .apply(FakeRequest(GET, s"/consignment/$consignmentId/transfer-summary").withCSRFToken)

      playStatus(transferSummaryPage) mustBe OK
      contentType(transferSummaryPage) mustBe Some("text/html")
      contentAsString(transferSummaryPage) must include("transferSummary.header")
      contentAsString(transferSummaryPage) must include("transferSummary.seriesReference")
      contentAsString(transferSummaryPage) must include("transferSummary.transferringBody")
      contentAsString(transferSummaryPage) must include("transferSummary.filesUploadedForTransfer")
      contentAsString(transferSummaryPage) must include("transferSummary.openRecords")
      contentAsString(transferSummaryPage) must include("transferSummary.transferLegalOwnership")
    }

    "return a redirect to the auth server with an unauthenticated user" in {
      val controller = new TransferSummaryController(getUnauthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)
      val transferSummaryPage = controller.transferSummary(consignmentId).apply(FakeRequest(GET, "/consignment/123/transfer-summary"))

      redirectLocation(transferSummaryPage).get must startWith("/auth/realms/tdr/protocol/openid-connect/auth")
      playStatus(transferSummaryPage) mustBe FOUND
    }

    "return a not found error page if an existing consignment does not have a transfer summary yet" in {
      val client = new GraphQLConfiguration(app.configuration).getClient[gc.Data, gc.Variables]()
      val data: client.GraphqlData = client.GraphqlData(Some(gc.Data(None)), List())
      val dataString: String = data.asJson.printWith(Printer(dropNullValues = false, ""))
      wiremockServer.stubFor(post(urlEqualTo("/graphql"))
        .willReturn(okJson(dataString)))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummaryPage = controller.transferSummary(consignmentId)
        .apply(FakeRequest(GET, s"/consignment/$consignmentId/transfer-summary").withCSRFToken)

      playStatus(transferSummaryPage) mustBe NOT_FOUND
      contentAsString(transferSummaryPage) must include("This page doesn't exist")
    }

    "throws an authorisation exception when the user does not have permission to see a consignment's transfer summary" in {
      val client = new GraphQLConfiguration(app.configuration).getClient[gc.Data, gc.Variables]()
      val graphQlError = GraphQLClient.Error("Error", Nil, Nil, Some(Extensions(Some("NOT_AUTHORISED"))))
      val data: client.GraphqlData = client.GraphqlData(Some(gc.Data(None)), List(graphQlError))
      val dataString: String = data.asJson.printWith(Printer(dropNullValues = false, ""))
      wiremockServer.stubFor(post(urlEqualTo("/graphql"))
        .willReturn(okJson(dataString)))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummaryPage = controller.transferSummary(consignmentId)
        .apply(FakeRequest(GET, s"/consignment/$consignmentId/transfer-summary").withCSRFToken)

      val failure: Throwable = transferSummaryPage.failed.futureValue

      failure mustBe an[AuthorisationException]
    }

    "display errors when an invalid form is submitted" in {
      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummarySubmit = controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary").withCSRFToken)

      playStatus(transferSummarySubmit) mustBe BAD_REQUEST
      contentAsString(transferSummarySubmit) must include("govuk-error-message")
      contentAsString(transferSummarySubmit) must include("error")
    }

    "redirects when a valid form is submitted" in {
      wiremockExportServer.stubFor(post(urlEqualTo(s"/export/$consignmentId"))
        .willReturn(okJson("{}")))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummarySubmit: Result = controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary")
          .withFormUrlEncodedBody(("openRecords", "true"), ("transferLegalOwnership", "true"))
          .withCSRFToken
        ).futureValue

      transferSummarySubmit.header.status should equal(303)
    }

    "redirects correctly when the call to the export api fails" in {
      wiremockExportServer.stubFor(post(urlEqualTo(s"/export/$consignmentId"))
        .willReturn(serverError()))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummarySubmit: Result = controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary")
          .withFormUrlEncodedBody(("openRecords", "true"), ("transferLegalOwnership", "true"))
          .withCSRFToken
        ).futureValue

      transferSummarySubmit.header.status should equal(303)
      transferSummarySubmit.header.headers("Location").contains("exportTriggered=false") should be(true)
    }

    "calls the export api when a valid form is submitted" in {
      wiremockExportServer.stubFor(post(urlEqualTo(s"/export/$consignmentId"))
        .willReturn(okJson("{}")))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary")
          .withFormUrlEncodedBody(("openRecords", "true"), ("transferLegalOwnership", "true"))
          .withCSRFToken
        ).futureValue

      wiremockExportServer.getAllServeEvents.size() should equal(1)
    }

    "redirects correctly when the call to the graphql api fails" in {
      val client = new GraphQLConfiguration(app.configuration).getClient[ut.Data, ut.Variables]()
      wiremockServer.stubFor(post(urlEqualTo("/graphql"))
        .willReturn(serverError()))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      val transferSummarySubmit: Result = controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary")
          .withFormUrlEncodedBody(("openRecords", "true"), ("transferLegalOwnership", "true"))
          .withCSRFToken
        ).futureValue

      transferSummarySubmit.header.status should equal(303)
      transferSummarySubmit.header.headers("Location").contains("exportTriggered=false") should be(true)
    }

    "calls the graphql api when a valid form is submitted" in {
      val client = new GraphQLConfiguration(app.configuration).getClient[ut.Data, ut.Variables]()
      val data: client.GraphqlData = client.GraphqlData(Some(ut.Data(Option(1))), List())
      val dataString: String = data.asJson.printWith(Printer(dropNullValues = false, ""))
      wiremockServer.stubFor(post(urlEqualTo("/graphql"))
        .willReturn(okJson(dataString)))

      val controller = new TransferSummaryController(getAuthorisedSecurityComponents, new GraphQLConfiguration(app.configuration),
        getValidKeycloakConfiguration, exportService(app.configuration), langs)

      controller.transferSummarySubmit(consignmentId)
        .apply(FakeRequest(POST, s"/consignment/$consignmentId/transfer-summary")
          .withFormUrlEncodedBody(("openRecords", "true"), ("transferLegalOwnership", "true"))
          .withCSRFToken
        ).futureValue

      wiremockServer.getAllServeEvents.size() should equal(1)
    }
  }
}
