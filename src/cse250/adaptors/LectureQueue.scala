/**
 * cse250.adaptors.LectureQueue.scala
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

class LectureQueue[A]
  extends cse250.types.mutable.QueueADT[A] {
  private val _store = new cse250.list.SinglyLinkedList[A]

  override def enqueue(elem: A): Unit = _store.insert(_store.length, elem)

  override def front: A = _store(0)

  override def dequeue: A = _store.remove(0)

  override def isEmpty: Boolean = _store.length == 0
}
