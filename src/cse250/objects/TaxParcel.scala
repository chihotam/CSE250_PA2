/**
 * cse250.objects.TaxParcel.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

import scala.collection.mutable.{HashMap, Map}

class TaxParcel {
  val parcelInfo: Map[String, String] = new HashMap[String, String]

  override def toString: String = {
    TaxParcel.HEADERS.map(h => parcelInfo.getOrElse(h, "")).addString(new StringBuilder(), ",").result()
  }

  // Scala Cookbook: Equals
  // https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch04s16.html
  def canEqual(a: Any) = a.isInstanceOf[TaxParcel]

  override def equals(that: Any): Boolean =
    that match {
      case that: TaxParcel => that.canEqual(this) && this.hashCode == that.hashCode
      case _               => false
    }

  override def hashCode: Int = {
    parcelInfo.hashCode
  }
}

object TaxParcel {
  val HEADERS = Array("SBL","TAX DISTRICT","PRINT KEY","FRONT","DEPTH","PROPERTY CLASS","PROPERTY CLASS DESCRIPTION","PREVIOUS PROPERTY CLASS","OWNER1","OWNER2","HOUSE NUMBER","STREET","CITY","STATE","ZIP CODE (5-DIGIT)","LAND VALUE","TOTAL VALUE","SALE PRICE","YEAR BUILT","# OF BEDS","# OF BATHS","COUNCIL DISTRICT","POLICE DISTRICT","NEIGHBORHOOD","LATITUDE","LONGITUDE","LOCATION")
}