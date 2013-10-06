package org.dbtoaster.dbtoasterlib {
  
  /** This object defines some additional implicit conversions that are being
    * used by queries.
    */
  object ImplicitConversions {
    implicit def boolToLong(b: Boolean): Long = if(b) 1L else 0L
  }
} 
