package services

import java.util.UUID

import com.nimbusds.oauth2.sdk.token.BearerAccessToken
import configuration.GraphQLConfiguration
import graphql.codegen.AddConsignment.addConsignment
import graphql.codegen.GetConsignment.getConsignment
import graphql.codegen.GetFileCheckProgress.getFileCheckProgress
import graphql.codegen.types.AddConsignmentInput
import graphql.codegen.{AddConsignment, GetConsignment, GetFileCheckProgress}
import javax.inject.{Inject, Singleton}
import services.ApiErrorHandling._
import sttp.client.{NothingT, SttpBackend}
import configuration.GraphqlBackend._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConsignmentService @Inject()(val graphqlConfiguration: GraphQLConfiguration)
                                  (implicit val ec: ExecutionContext)  {

  private val getConsignmentClient = graphqlConfiguration.getClient[getConsignment.Data, getConsignment.Variables]()
  private val addConsignmentClient = graphqlConfiguration.getClient[addConsignment.Data, addConsignment.Variables]()
  private val getConsignmentFileCheckClient = graphqlConfiguration.getClient[getFileCheckProgress.Data, getFileCheckProgress.Variables]()

  def consignmentExists(consignmentId: UUID,
                        token: BearerAccessToken)(implicit backend: SttpBackend[Future, Nothing, NothingT]): Future[Boolean] = {
    val variables: getConsignment.Variables = new GetConsignment.getConsignment.Variables(consignmentId)

    sendApiRequest(getConsignmentClient, getConsignment.document, token, variables)
      .map(data => data.getConsignment.isDefined)
  }

  def createConsignment(seriesId: UUID, token: BearerAccessToken): Future[addConsignment.AddConsignment] = {
    val addConsignmentInput: AddConsignmentInput = AddConsignmentInput(seriesId)
    val variables: addConsignment.Variables = AddConsignment.addConsignment.Variables(addConsignmentInput)

    sendApiRequest(addConsignmentClient, addConsignment.document, token, variables)
      .map(data => data.addConsignment)
  }

  def getConsignmentFileChecks(consignmentId: UUID, token: BearerAccessToken): Future[getFileCheckProgress.GetConsignment] = {
    val variables: getFileCheckProgress.Variables = new GetFileCheckProgress.getFileCheckProgress.Variables(consignmentId)

    sendApiRequest(getConsignmentFileCheckClient, getFileCheckProgress.document, token, variables)
      .map(data => data.getConsignment.get)
  }
}
