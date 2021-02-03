/**
 * cse250.types.mutable.ListADT.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Do not modify this file.
 */
package cse250.types.mutable

trait ListADT[A] {
  /** Gets the element at the specified index. */
  def apply(idx: Int): A

  /** Replaces element at given index with a new value. */
  def update(idx: Int, elem: A): Unit

  /** Inserts element at given index. */
  def insert(idx: Int, elem: A): Unit

  /** Removes element at given index and returns the value removed. */
  def remove(idx: Int): A

  /** Returns an Iterator that can be used only once.
   *  Get access to view all elements, in order, contained within the sequence. */
  def iterator: Iterator[A]

  override def toString: String = iterator.toList.addString(new StringBuilder,"mutable.ListADT(",",",")").result()
}
