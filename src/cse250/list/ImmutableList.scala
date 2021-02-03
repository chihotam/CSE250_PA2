/**
 * cse250.list.ImmutableList.scala
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
package cse250.list

import scala.annotation.tailrec

sealed trait ImmutableLinkedList[+A] extends collection.immutable.Seq[A]
  with cse250.types.immutable.ListADT[A] {

  override def head: A = this match {
    case n: ListNode[A] => n._value
    case EmptyList      => throw new IllegalArgumentException("EmptyList has not head value.")
  }

  override def tail: ImmutableLinkedList[A] = this match {
    case n: ListNode[A] => n._next
    case EmptyList      => throw new IllegalArgumentException("EmptyList has not tail reference.")
  }

  @tailrec
  private def lengthHelper(acc: Int): Int = this match {
    case _: ListNode[A] => this.tail.lengthHelper(acc + 1)
    case EmptyList      => acc
  }

  override def length: Int = this match {
    case _: ListNode[A] => this.tail.lengthHelper(1)
    case EmptyList      => 0
  }

  override def isEmpty = this match {
    case _: ListNode[A] => false
    case EmptyList      => true
  }

  override def apply(idx: Int): A = {
    if (idx == 0) head
    else tail(idx - 1)
  }

  override def updated[B >: A](idx: Int, elem: B): ImmutableLinkedList[B] = {
    if (idx == 0) ListNode[B](elem, tail)
    else ListNode[B](head, tail.updated(idx - 1, elem))
  }

  override def inserted[B >: A](idx: Int, elem: B): ImmutableLinkedList[B] = {
    if (idx == 0) ListNode[B](elem, this)
    else ListNode[B](head, tail.inserted(idx - 1, elem))
  }

  override def removed(idx: Int): ImmutableLinkedList[A] = {
    if (idx == 0) tail
    else ListNode[A](head, tail.removed(idx - 1))
  }

  override def prepended[B >: A](elem: B): ImmutableLinkedList[B] = ListNode[B](elem, this)

  def ::[B >: A](elem: B): ImmutableLinkedList[B] = ListNode[B](elem, this)

  override def iterator: Iterator[A] = new Iterator[A] {
    var _currentList = ImmutableLinkedList.this

    override def hasNext: Boolean = !_currentList.isEmpty

    override def next: A = {
      val retval = _currentList.head
      _currentList = _currentList.tail
      retval
    }
  }
}

case class ListNode[A](_value: A, _next: ImmutableLinkedList[A]) extends ImmutableLinkedList[A]

object EmptyList extends ImmutableLinkedList[Nothing]