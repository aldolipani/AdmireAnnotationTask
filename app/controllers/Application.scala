package controllers

import controllers.Dashboard._
import play.api.mvc._
import models.User
import play.Logger
import play.i18n.Messages
import play.api.data._
import play.api.data.Forms._

object Application extends Controller{

  def goHome = Redirect(routes.Application.index)
  def goDashboard = Redirect(routes.Dashboard.index)

  var message = ""

  def index = Action { request =>
    val username = request.session.get(Security.username)
    if (username != None) {
      val user: User = User.findByUsername(username.get)
      if (user != null) {
        Redirect(routes.Dashboard.index)
      } else {
        Logger.debug("Invalid session credentials")
        Ok(views.html.index.render(registerForm, loginForm, message))
      }
    }else{
      Ok(views.html.index.render(registerForm, loginForm, message))
    }
  }

  def pageNotFound(url: String) = Action { request =>
    Ok(views.html.message.render(
      "Page " + url + " not found! :-("))
  }

  def authenticate = Action { implicit request =>
     loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.index.render(registerForm, formWithErrors, "")),
      user => goDashboard.withSession(Security.username -> user._1))}

  def logout = Action { implicit request =>  Redirect("/").withNewSession }

  val loginForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    ).verifying(Messages.get("invalid.username.or.password"), fields => fields match {
      case (u, p) => User.authenticate(u, p) != null
    })
  )

  val registerForm = Form(
    tuple(
      "username" -> nonEmptyText,
      "password" -> nonEmptyText
    ).verifying(Messages.get("error.username.already.exist"), fields => fields match {
      case (u, p) => User.findByUsername(u) == null
    })
  )
}
