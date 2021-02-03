/**
 * cse250.pa2.SortedList.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: chihotam
 * Person#: 50301678
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.list.{ImmutableLinkedList,EmptyList,ListNode}
import cse250.adaptors.{LectureQueue,LectureStack}
import cse250.objects.TaxParcel


class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  var stack: LectureStack[ImmutableLinkedList[A]] = new LectureStack[ImmutableLinkedList[A]]
  stack.push(cse250.list.EmptyList)
  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  // You may add member variables as you wish.

  /** Gets element at position idx within the list. */
  override def apply(idx: Int): A = {
    if(idx < 0 || idx >= length){
      throw new IllegalArgumentException("OutOfBound Index.")
    }
    else{
      _storageList.apply(idx)
    }
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = {
    _storageList.length
  }

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = {
    _storageList.iterator
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    if(stack.isEmpty){
      stack.push(cse250.list.EmptyList)
    }
    if(length == 0){
      _storageList = _storageList.inserted(0,elem)
    }
    else{
      val iter = iterator
      var counter = 0
      var change = false
      while(iter.hasNext && !change){
        val next = iter.next
        if(_comp.gteq(next,elem)){
          _storageList = _storageList.inserted(counter,elem)
          change = true
        }
        else if(!iter.hasNext && _comp.lt(next,elem)){
          _storageList = _storageList.inserted(length,elem)
          change = true
        }
        counter += 1
      }
    }
    stack.push(_storageList)
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return the number of copies removed.
   */
  def remove(elem: A): Int = {
    if(length == 0){
      return 0
    }
    var total = 0
    var temp = _storageList
    var counter = 0
    val iter = iterator
    while(iter.hasNext){
      if(_comp.equiv(iter.next,elem)){
        temp = temp.removed(counter-total)
        total += 1
      }
      counter += 1
    }
    _storageList = temp
    if(_storageList != stack.top){
      stack.push(_storageList)
    }
    total
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    val old = stack.top
    val queue: LectureQueue[A] = new LectureQueue[A]
    while(!operations.isEmpty){
      if(operations.front._1 == "insert"){
        insert(operations.front._2)
        queue.enqueue(operations.front._2)
      }
      else{
        val temp: LectureQueue[A] = queue
        var boo = false
        while(!temp.isEmpty){
          if(temp.dequeue == operations.front._2) {
            _storageList = _storageList.removed(indexOf(operations.front._2))
            boo = true
          }
        }
        if(!boo) {
          remove(operations.front._2)
        }
      }
      operations.dequeue
      if(stack.top != old){
        stack.pop
      }
    }
    if(_storageList != stack.top){
      stack.push(_storageList)
    } 
  }

  /** Undo the last modification, if any change has been made.
   *  If no change to undo exists, raise an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    if(stack.isEmpty){
      throw new IllegalArgumentException("No Modification.")
    }
    else{
      stack.pop
      if(stack.isEmpty){
        throw new IllegalArgumentException("No Modification.")
      }
      else{
        _storageList = stack.top
      }
    }
  }
}

object NewIntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    if((x % 2 == 0 && y % 2 == 0) || (x % 2 != 0 && y % 2 != 0)){
      if(x > y){
        return -1
      }
      if(x < y){
        return 1
      }
    }
    if(x % 2 == 0 && y % 2 != 0){
      return -1
    }
    if(x % 2 != 0 && y % 2 == 0){
      return 1
    }
    0
  }
}

object TaxParcelStreetGroupingOrdering extends Ordering[TaxParcel] {
  def compare(x: TaxParcel, y: TaxParcel): Int = {
    if(x.parcelInfo("STREET") > y.parcelInfo("STREET")){
      1
    }
    else if(x.parcelInfo("STREET") < y.parcelInfo("STREET")){
      -1
    }
    else{
      0
    }
  }
}