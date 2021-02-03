/**
 * cse250.types.immutable.ListADT.scala
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
package cse250.types.immutable

trait ListADT[+A] {
  /** Gets the element at the specified index. */
  def apply(idx: Int): A

  /** Returns a copy of the list with the element replaced at the given index. */
  def updated[B>:A](idx: Int, elem: B): ListADT[B]

  /** Returns a copy of the list with the element inserted at the given index. */
  def inserted[B>:A](idx: Int, elem: B): ListADT[B]

  /** Returns a copy of the list with the element removed at the given index. */
  def removed(idx: Int): ListADT[A]

  /** Returns an Iterator that can be used only once.
   *  Get access to view all elements, in order, contained within the sequence. */
  def iterator: Iterator[A]

  override def toString: String = iterator.toList.addString(new StringBuilder,"immutable.ListADT(",",",")").result()
}