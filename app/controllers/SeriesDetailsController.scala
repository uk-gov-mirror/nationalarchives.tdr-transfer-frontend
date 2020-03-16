package controllers

import java.util.UUID

import auth.TokenSecurity
import com.amazonaws.auth.policy.{Policy, Resource, Statement}
import com.amazonaws.auth.policy.Statement.Effect
import com.amazonaws.auth.policy.actions.S3Actions
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials, BasicSessionCredentials}
import com.amazonaws.auth.profile.ProfileCredentialsProvider
import com.amazonaws.regions.Regions
import com.amazonaws.services.cognitoidentity.{AmazonCognitoIdentityClient, AmazonCognitoIdentityClientBuilder}
import com.amazonaws.services.cognitoidentity.model.{GetCredentialsForIdentityRequest, GetOpenIdTokenForDeveloperIdentityRequest}
import com.amazonaws.services.s3.model.{ListObjectsV2Request, ListObjectsV2Result}
import com.amazonaws.services.s3.{AmazonS3, AmazonS3ClientBuilder}
import com.amazonaws.services.securitytoken.model.{Credentials, GetFederationTokenRequest, GetFederationTokenResult}
import com.amazonaws.services.securitytoken.{AWSSecurityTokenService, AWSSecurityTokenServiceClientBuilder}
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
import play.api.mvc.{Action, AnyContent, Request, RequestHeader, Result}

import scala.jdk.CollectionConverters._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SeriesDetailsController @Inject()(val controllerComponents: SecurityComponents,
                                        val graphqlConfiguration: GraphQLConfiguration,
                                        val keycloakConfiguration: KeycloakConfiguration
                                        )(implicit val ec: ExecutionContext) extends TokenSecurity with I18nSupport {

  private val secureAction = Secure("OidcClient")
  private val getSeriesClient = graphqlConfiguration.getClient[getSeries.Data, getSeries.Variables]()
  private val addConsignmentClient = graphqlConfiguration.getClient[addConsignment.Data, addConsignment.Variables]()
  private val amazonCognitoClient = AmazonCognitoIdentityClientBuilder.standard()

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
    getDeveloperProviderCredentials(request)
    getSeriesDetails(request, Ok, selectedSeriesForm)
  }

  def getDeveloperProviderCredentials(request: Request[AnyContent])(implicit requestHeader: RequestHeader) = {

    val userId: String = request.token.userId.getOrElse("")

    val logins = Map("auth.tdr-integration.nationalarchives.gov.uk" -> userId).asJava

    val tokenRequest: GetOpenIdTokenForDeveloperIdentityRequest = new GetOpenIdTokenForDeveloperIdentityRequest
    tokenRequest.setIdentityPoolId("eu-west-2:f2d20d5e-ffcb-4446-b70a-579c762898ec")
    tokenRequest.setTokenDuration(900L)
    tokenRequest.setLogins(logins)

    val client = amazonCognitoClient
      .withRegion(Regions.EU_WEST_2)
      .withCredentials(new ProfileCredentialsProvider("sandbox"))
      .build()
    val result = client.getOpenIdTokenForDeveloperIdentity(tokenRequest)

    val cognitoLogins = Map("cognito-identity.amazonaws.com" -> result.getToken).asJava

    val credReq: GetCredentialsForIdentityRequest = new GetCredentialsForIdentityRequest
    credReq.setIdentityId(result.getIdentityId)
    credReq.setLogins(cognitoLogins)

    val tempCreds = client.getCredentialsForIdentity(credReq)
    val tempAccessKey: String = tempCreds.getCredentials.getAccessKeyId
    val tempSecretKey: String = tempCreds.getCredentials.getSecretKey
    val tempSession: String = tempCreds.getCredentials.getSessionToken

    println("Access Key: " + tempAccessKey)
    println("Secret Key: " + tempSecretKey)
    println("Session Key: " + tempSession)

    val sessionCredentials: BasicSessionCredentials = new BasicSessionCredentials(
      tempAccessKey, tempSecretKey, tempSession)

    val s3Client: AmazonS3 = AmazonS3ClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(sessionCredentials))
        .withRegion(Regions.EU_WEST_2)
        .build()

    val listObjectsReq: ListObjectsV2Request = new ListObjectsV2Request()
      .withBucketName("tktest-upload")
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
case class TemporaryCredentials(accessKeyId: String, secretAccessKey: String, sessionToken: String)
case class SelectedSeriesData (seriesId: String)
