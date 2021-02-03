/**
 * cse250.adaptors.LectureStack.scala
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
package cse250.adaptors

class LectureStack[A]
  extends cse250.types.mutable.StackADT[A] {
  private val _store = new cse250.list.DoublyLinkedList[A]

  override def push(elem: A): Unit = _store.insert(_store.length, elem)

  override def top: A = _store(_store.length - 1)

  override def pop: A = _store.remove(_store.length - 1)

  override def isEmpty: Boolean = _store.length == 0
}
