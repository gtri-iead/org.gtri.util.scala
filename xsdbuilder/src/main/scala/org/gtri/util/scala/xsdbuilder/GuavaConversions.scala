//package org.gtri.util.scala.xsdbuilder
//
//import com.google.common.collect.{ImmutableList, ImmutableMap, ImmutableSet}
//
///**
// * Created with IntelliJ IDEA.
// * User: Lance
// * Date: 12/4/12
// * Time: 7:54 AM
// * To change this template use File | Settings | File Templates.
// */
//object GuavaConversions {
//  import scala.language.implicitConversions
//
//  implicit def setToGuavaImmutableSet[A](set : Set[A]) : ImmutableSet[A] = {
//    if(set.isEmpty) {
//      ImmutableSet.of()
//    } else {
//      val builder = ImmutableSet.builder[A]()
//      for(item <- set) {
//        builder.add(item)
//      }
//      builder.build()
//    }
//  }
//  implicit def mapToGuavaImmutableMap[K,V](map : Map[K,V]) : ImmutableMap[K,V] = {
//    if(map.isEmpty) {
//      ImmutableMap.of()
//    } else {
//      val builder = ImmutableMap.builder[K,V]()
//      for((key,value) <- map) {
//        builder.put(key, value)
//      }
//      builder.build()
//    }
//  }
//  implicit def setToGuavaImmutableList[A](list : List[A]) : ImmutableList[A] = {
//    if (list.isEmpty) {
//      ImmutableList.of()
//    } else {
//      val builder = ImmutableList.builder[A]()
//      for(item <- list) {
//        builder.add(item)
//      }
//      builder.build()
//    }
//  }
//  import scala.collection.JavaConversions._
//  implicit def guavaImmutableSetToSet[A](set : ImmutableSet[A]) : Set[A] = {
//    if(set.isEmpty) {
//      Set.empty
//    } else {
//      set.iterator.toSet
//    }
//  }
//  implicit def guavaImmutableMapToMap[K,V](map : ImmutableMap[K,V]) : Map[K,V] = {
//    if(map.isEmpty) {
//      Map.empty
//    } else {
//      map.iterator.toMap
//    }
//  }
//  implicit def guavaImmutableListToList[A](list : ImmutableList[A]) : List[A] = {
//    if(list.isEmpty) {
//      List.empty
//    } else {
//      list.iterator.toList
//    }
//  }
//}
