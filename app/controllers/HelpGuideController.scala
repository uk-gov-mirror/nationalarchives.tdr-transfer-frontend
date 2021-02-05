package controllers

import auth.OidcSecurity
import javax.inject.{Inject, Singleton}
import org.pac4j.play.scala.SecurityComponents
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Request}

@Singleton
class HelpGuideController @Inject()(val controllerComponents: SecurityComponents) extends OidcSecurity with I18nSupport  {
  def helpGuide(): Action[AnyContent] = secureAction { implicit request: Request[AnyContent] =>
    Ok(views.html.helpGuide())
  }
}