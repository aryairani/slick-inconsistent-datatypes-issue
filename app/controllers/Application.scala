package controllers

import play.api._
import play.api.mvc._
import play.api.templates.Html

object Application extends Controller {

  def foo(x: Option[model.CityId]) = Action {
    Ok(views.html.main("Result:")(Html(model.searchOptionCityId(x).headOption.getOrElse("No results."))))
  }

  def working = foo(model.getFirstCityId)

  def broken = foo(None)

}