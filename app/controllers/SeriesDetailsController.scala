package controllers

import java.util.UUID

import auth.TokenSecurity
import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicSessionCredentials}
import com.amazonaws.regions.Regions
import com.amazonaws.services.cognitoidentity.AmazonCognitoIdentityClientBuilder
import com.amazonaws.services.cognitoidentity.model.{GetCredentialsForIdentityRequest, GetOpenIdTokenForDeveloperIdentityRequest}
import com.amazonaws.services.s3.model.{ListObjectsV2Request, ListObjectsV2Result}
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import configuration.{GraphQLConfiguration, KeycloakConfiguration}
import graphql.codegen.AddConsignment
import graphql.codegen.AddConsignment.addConsignment
import graphql.codegen.GetSeries.getSeries
import graphql.codegen.types.AddConsignmentInput
import javax.inject.{Inject, Singleton}
import org.pac4j.play.scala.SecurityComponents
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.I18nSupport
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

@Singleton
class SeriesDetailsController @Inject()(val controllerComponents: SecurityComponents,
                                        val graphqlConfiguration: GraphQLConfiguration,
                                        val keycloakConfiguration: KeycloakConfiguration
                                        )(implicit val ec: ExecutionContext) extends TokenSecurity with I18nSupport {

  private val getSeriesClient = graphqlConfiguration.getClient[getSeries.Data, getSeries.Variables]()
  private val addConsignmentClient = graphqlConfiguration.getClient[addConsignment.Data, addConsignment.Variables]()


  val selectedSeriesForm = Form(
    mapping(
      "series" -> nonEmptyText
    )(SelectedSeriesData.apply)(SelectedSeriesData.unapply)
  )

  private def getSeriesDetails(request: Request[AnyContent], status: Status, form: Form[SelectedSeriesData])(implicit requestHeader: RequestHeader) = {
    val userTransferringBody: Option[String] = request.token.transferringBody
    val variables: getSeries.Variables = new getSeries.Variables(userTransferringBody)

    getSeriesClient.getResult(request.token.bearerAccessToken, getSeries.document, Some(variables)).map(data => {
      if (data.data.isDefined) {
        val seriesData: Seq[(String, String)] = data.data.get.getSeries.map(s => (s.seriesid.toString, s.code.getOrElse("")))
        status(views.html.seriesDetails(seriesData, form))
      } else {
        BadRequest(views.html.error(data.errors.map(e => e.message).mkString))
      }
    })
  }

  def seriesDetails(): Action[AnyContent] = secureAction.async { implicit request: Request[AnyContent] =>
    testDeveloperProviderIdentityS3AccessControls(request)
    getSeriesDetails(request, Ok, selectedSeriesForm)
  }

  def testDeveloperProviderIdentityS3AccessControls(request: Request[AnyContent])(implicit requestHeader: RequestHeader) = {

    val amazonCognitoClient = AmazonCognitoIdentityClientBuilder.standard()
    val testBucketName: String = "tktest-upload"
    val cognitoUserProfileName: String = "sandbox"
    val identityPoolId: String = "eu-west-2:f2d20d5e-ffcb-4446-b70a-579c762898ec"
    val developerProviderName: String = "auth.tdr-integration.nationalarchives.gov.uk"
    val cognitoIdentityName: String = "cognito-identity.amazonaws.com"

    //Assume user already authenticated
    val userId: String = request.token.userId.getOrElse("")

    val developerProviderLogins = Map(developerProviderName -> userId).asJava

    val tokenRequest: GetOpenIdTokenForDeveloperIdentityRequest = new GetOpenIdTokenForDeveloperIdentityRequest
    tokenRequest.setIdentityPoolId(identityPoolId)
    tokenRequest.setTokenDuration(900L)
    tokenRequest.setLogins(developerProviderLogins)

    val client = amazonCognitoClient
      .withRegion(Regions.EU_WEST_2)
      .withCredentials(new ProfileCredentialsProvider(cognitoUserProfileName))
      .build()
    val result = client.getOpenIdTokenForDeveloperIdentity(tokenRequest)

    val cognitoLogins = Map(cognitoIdentityName -> result.getToken).asJava

    val credReq: GetCredentialsForIdentityRequest = new GetCredentialsForIdentityRequest
    credReq.setIdentityId(result.getIdentityId)
    credReq.setLogins(cognitoLogins)

    val tempCreds = client.getCredentialsForIdentity(credReq).getCredentials

    val sessionCredentials: BasicSessionCredentials = new BasicSessionCredentials(
      tempCreds.getAccessKeyId, tempCreds.getSecretKey, tempCreds.getSessionToken)

    val s3Client: AmazonS3 = AmazonS3ClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(sessionCredentials))
        .withRegion(Regions.EU_WEST_2)
        .build()

    //Test uploading an object
    s3Client.putObject(testBucketName, result.getIdentityId + "/thisisatest", "This is a test")

    //Test listing out objects uploaded by user
    val listObjectsReq: ListObjectsV2Request = new ListObjectsV2Request()
      .withBucketName(testBucketName)
      .withPrefix(result.getIdentityId)
    val listResult: ListObjectsV2Result = s3Client.listObjectsV2(listObjectsReq)

    println(listResult.getObjectSummaries.toString)
  }

  def seriesSubmit(): Action[AnyContent] =  secureAction.async { implicit request: Request[AnyContent] =>
    val formValidationResult: Form[SelectedSeriesData] = selectedSeriesForm.bindFromRequest

    val errorFunction: Form[SelectedSeriesData] => Future[Result] = { formWithErrors: Form[SelectedSeriesData] =>
      getSeriesDetails(request, BadRequest, formWithErrors)
    }

    val successFunction: SelectedSeriesData => Future[Result] = { formData: SelectedSeriesData =>
      val userId = request.token.userId
      val addConsignmentInput: AddConsignmentInput = AddConsignmentInput(formData.seriesId.toLong, UUID.fromString(userId.get))
      val variables: addConsignment.Variables = AddConsignment.addConsignment.Variables(addConsignmentInput)

      addConsignmentClient.getResult(request.token.bearerAccessToken, addConsignment.document, Some(variables)).map(data => {
        if(data.data.isDefined) {
          Redirect(routes.TransferAgreementController.transferAgreement(data.data.get.addConsignment.consignmentid.get))
        } else {
          BadRequest(views.html.error(data.errors.map(e => e.message).mkString))
        }
      })
    }
    formValidationResult.fold(
      errorFunction,
      successFunction
    )
  }
}

case class SelectedSeriesData (seriesId: String)
