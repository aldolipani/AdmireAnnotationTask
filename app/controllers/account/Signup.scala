package controllers.account

import models.User
import play.api.mvc.{Security, Controller, Action}
import models.utils.Hash
import controllers.Application

/**
 * Signup to PlayStartApp : save and send confirm mail.
 * <p/>
 * User: yesnault
 * Date: 31/01/12
 */
object Signup extends Controller{

  def createFormOnly = Action { request =>
    Ok(views.html.register.render(Application.registerForm))
  }

  def save = Action { implicit request =>
    Application.registerForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.index.render(formWithErrors, Application.loginForm, "")),
      form => {
        val user: User = new User
        user.email = form._1
        user.password = Hash.createPassword(form._2)
        user.save
        Redirect(controllers.routes.Dashboard.index).withSession(Security.username -> form._1)})}
}


