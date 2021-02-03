/**
 * cse250.pa2.SortedListTests.scala
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


import cse250.adaptors.LectureQueue
import cse250.list.EmptyList
import cse250.types.mutable.QueueADT
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._

class SortedListTests extends FlatSpec with BeforeAndAfter {
  behavior of "apply"
  it should "get the element at the index given" in {
    val myList = new SortedList[Int]
    assertThrows [IllegalArgumentException] {
      myList.apply(0)
    }
    myList.insert(2)
    assert(myList.apply(0) == 2)
    assertThrows [IllegalArgumentException] {
      myList.apply(1)
    }
    myList.insert(3)
    assert(myList.apply(1) == 3)
    assertThrows [IllegalArgumentException] {
      myList.apply(2)
    }
    myList.insert(1)
    assert(myList.apply(0) == 1)
    assert(myList.apply(1) == 2)
    assert(myList.apply(2) == 3)
    assert(myList.length == 3)
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 2)
    assert(myList._storageList(2) == 3)
    assertThrows [IllegalArgumentException] {
      myList.apply(3)
    }
  }

  behavior of "length"
  it should "get the number of elements stored within the list" in {
    val myList = new SortedList[Int]
    assert(myList.length == 0)
    myList.insert(2)
    assert(myList.length == 1)
    myList.insert(3)
    myList.insert(1)
    assert(myList.length == 3)
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 2)
    assert(myList._storageList(2) == 3)
  }

  behavior of "iterator"
  it should "return an iterator with all the elements" in {
    val myList = new SortedList[Int]
    val lis: List[Int] = List(1,2,3)
    var counter = 0
    myList.insert(2)
    myList.insert(3)
    myList.insert(1)
    val iter = myList.iterator
    while(iter.hasNext){
      assert(iter.next == lis(counter))
      counter += 1
    }
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 2)
    assert(myList._storageList(2) == 3)
  }

  behavior of "insert"
  it should "insert given element into the list in non-decreasing order" in {
    val myList = new SortedList[Int]
    myList.insert(2)
    myList.insert(3)
    myList.insert(1)
    myList.insert(2)
    assert(myList(0) == 1)
    assert(myList(1) == 2)
    assert(myList(2) == 2)
    assert(myList(3) == 3)
    assert(myList.length == 4)
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 2)
    assert(myList._storageList(2) == 2)
    assert(myList._storageList(3) == 3)
  }

  behavior of "remove"
  it should "remove all copies of the given element and return the number of copies" in {
    val myList = new SortedList[Int]
    myList.insert(2)
    myList.insert(3)
    myList.insert(1)
    myList.insert(2)
    assert(myList.remove(2) == 2)
    assert(myList(0) == 1)
    assert(myList(1) == 3)
    assert(myList.length == 2)
    assert(myList.remove(2) == 0)
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 3)
  }

  behavior of "processBatch"
  it should "do all the operations in the given queue and there are no undo" in {
    val myList = new SortedList[Int]
    val jobs = new LectureQueue[(String,Int)]
    jobs.enqueue("insert",2)
    jobs.enqueue("remove",2)
    jobs.enqueue("insert",3)
    jobs.enqueue("insert",2)
    jobs.enqueue("insert",1)
    jobs.enqueue("insert",4)
    jobs.enqueue("remove",4)
    myList.processBatch(jobs)
    assert(myList(0) == 1)
    assert(myList(1) == 2)
    assert(myList(2) == 3)
    assert(myList.length == 3)
    assert(myList._storageList(0) == 1)
    assert(myList._storageList(1) == 2)
    assert(myList._storageList(2) == 3)
    val myList2 = new SortedList[Int]
    myList2.insert(2)
    println(myList2.stack.top)
    val jobs2 = new LectureQueue[(String,Int)]
    jobs2.enqueue("remove",2)
    jobs2.enqueue("insert",2)
    myList2.processBatch(jobs)
    println(myList2.stack.top)
    assert(myList2.length == 1)
    myList2.undoLastModification()
    println(myList2.stack.top)
    assert(myList2.length == 0)
  }

  behavior of "undoLastModification"
  it should "undo the last modification" in {
    val myList = new SortedList[Int]
    assertThrows [IllegalArgumentException] {
      myList.undoLastModification()
    }
    myList.insert(1)
    println(myList.stack.top)
    myList.undoLastModification()
    assert(myList.length == 0)
    myList.insert(1)
    myList.remove(1)
    myList.undoLastModification()
    assert(myList(0) == 1)
    assert(myList.length == 1)
    val jobs = new LectureQueue[(String,Int)]
    jobs.enqueue("insert",2)
    jobs.enqueue("remove",2)
    myList.processBatch(jobs)
    myList.undoLastModification()
    assert(myList.length == 0)
    val jobs2 = new LectureQueue[(String,Int)]
    jobs2.enqueue("insert",1)
    jobs2.enqueue("insert",2)
    myList.processBatch(jobs2)
    myList.undoLastModification()
    assert(myList.length == 0)
    val jobs3 = new LectureQueue[(String,Int)]
    jobs3.enqueue("remove",1)
    jobs3.enqueue("insert",2)
    myList.processBatch(jobs3)
    myList.insert(3)
    myList.undoLastModification()
    assert(myList(0) == 2)
    assert(myList.length == 1)
    myList.undoLastModification()
    assert(myList._storageList == EmptyList)
    assertThrows [IllegalArgumentException] {
      myList.undoLastModification()
    }
  }





  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)
    assert(myList(0) == valToInsert)
  }

  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  it should "process two insertions and then undo both" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    myList.undoLastModification()
    assert(myList.length == 0)
  }
}
