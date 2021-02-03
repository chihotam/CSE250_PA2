/**
 * cse250.types.mutable.QueueADT.scala
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

trait QueueADT[A] {
  def enqueue(elem: A): Unit

  def front: A

  def dequeue: A

  def isEmpty: Boolean
}
