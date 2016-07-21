using UnityEngine;
using clojure.lang;
using System;
using System.Collections;
using System.Collections.Generic;

public class TimeLine : IEnumerator{
  public object[] fns;
  int idx = -1;
  public IFn predicate = null;
  public TimeLine(object[] list){
    fns = list;}
  public bool MoveNext(){
    if (idx == fns.Length) {
      return false;}
    else if (!(predicate == null) && RT.booleanCast(predicate.invoke())){
      return true;}
//    else if (idx > -1 && fns[idx] is IEnumerator){
//      Debug.Log(fns[idx]);
//      return false;}
    else if (idx + 1  < fns.Length) {
      idx++;
      try{
        predicate = (clojure.lang.IFn)fns[idx];}
      catch (System.InvalidCastException){
        predicate = null;}
      return true;}
    else {
      return false;}}
  public void Reset(){
    idx = -1;
    predicate = null;}
//  object IEnumerator.Current{
//    get{
//      return Current;}}
  public object Current{
    get{
      try{
        return fns[idx];}
      catch (System.IndexOutOfRangeException) {
        throw new InvalidOperationException();}}}}