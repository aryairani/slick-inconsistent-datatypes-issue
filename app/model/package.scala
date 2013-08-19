import play.api.db.DB
import play.api.Logger
import scala.slick.lifted.Parameters
import com.typesafe.slick.driver.oracle.OracleDriver.simple._
import scala.slick.session.Database.threadLocalSession
import play.api.Play.current


/**
 * Created with IntelliJ IDEA.
 * User: arya
 * Date: 8/18/13
 * Time: 1:00 PM
 * To change this template use File | Settings | File Templates.
 */
package object model {

  val db = Database.forDataSource(DB.getDataSource())

  def hextoraw[T<:TypedId:TypeMapper] = SimpleFunction.unary[String,T]("hextoraw")

  type WrappedIdType = Array[Byte]
  trait TypedId extends Any {
    def untypedId: WrappedIdType
    override def toString = untypedId.map("%02X" format _).mkString
  }

  sealed trait IdFactory[T <: TypedId] extends (WrappedIdType => T) {
    implicit val idMapper = MappedTypeMapper.base[T,WrappedIdType](_.untypedId, this)
  }

  case class AgencyId(untypedId: WrappedIdType) extends AnyVal with TypedId
  case class CityId(untypedId: WrappedIdType) extends AnyVal with TypedId

  implicit object AgencyId        extends IdFactory[AgencyId]
  implicit object CityId          extends IdFactory[CityId]


  abstract class TableIdName[T<:TypedId:TypeMapper, R](tblName: String) extends Table[R](tblName) {
    def id = column[T]("ID", O.PrimaryKey)
    def name = column[String]("NAME")
    def idName = id ~ name
    def optIdName = id.? ~ name
  }

  case class Agency(id: Option[AgencyId] = None, name: String, cityId: Option[CityId])
  object Agency extends TableIdName[AgencyId,Agency]("TB_AGENCY") {
    def cityId = column[Option[CityId]]("CITY_ID")
    def * = optIdName ~ cityId <> (Agency.apply _, Agency.unapply _)
  }

  case class City(id: Option[CityId], name: String)
  object City extends TableIdName[CityId,City]("TB_CITY") {
    def * = optIdName <> (City.apply _, City.unapply _)
  }

  def getFirstCityId = db withSession { (for (c <- City) yield c.id).firstOption }

  def searchOptionCityId(cityId: Option[CityId]) = {
    val query = for {
      cityIdFilter <- Parameters[Option[CityId]]
      agency <- Agency if (cityIdFilter.isNull || agency.cityId === cityIdFilter.get)
    } yield agency.name

//    Logger.info(query.selectStatement)
    // select x2."NAME" from "TB_AGENCY" x2 where (? is null) or (? = x2."CITY_ID")

//    query.apply(cityId)

    db withSession query.list(cityId) // Execution exception
  }

//  def searchOptionString(option: Option[String]) = {
//    val query = for {
//      str <- Parameters[Option[String]]
//      agency <- Agency if (str.isNull)
//    } yield agency.name
//
//    Logger.info(query.selectStatement)
//    // select x2."NAME" from "TB_AGENCY" x2 where (? is null) or (? = x2."CITY_ID")
//
//    db withSession query.list(option) // Execution exception
//  }
//
//  def searchOptionByteArray(option: Option[Array[Byte]]) = {
//    val query = for {
//      bytes <- Parameters[Option[Array[Byte]]]
//      agency <- Agency if (bytes.isNull || cityId)
//    } yield agency.name
//
//    Logger.info(query.selectStatement)
//    // select x2."NAME" from "TB_AGENCY" x2 where (? is null) or (? = x2."CITY_ID")
//
//    db withSession query.list(option) // Execution exception
//  }

//  def doSearchBroken(foo: Option[CityId]) = {
//    val query = for {
//      cityIdFilter <- Parameters[Option[CityId]]
//      agency <- Agency if (cityIdFilter.isNull || cityIdFilter.get === agency.cityId)
//    } yield agency.name
//
//    Logger.info(query.selectStatement)
//    // select x2."NAME" from "TB_AGENCY" x2 where (? is null) or (? = x2."CITY_ID")
//
//    db withSession query.list(cityId) // Execution exception
//  }



}