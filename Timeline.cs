using UnityEngine;
using System.Collections;

public class TimeLine : IEnumerator {
	public object[] fns;
	int idx = -1;
	public TimeLine(object[] list) {
		fns = list;}
	public bool MoveNext() {
		idx++;
		return (idx < fns.Length);}
	public void Reset() {
		idx = -1;}
	object IEnumerator.Current {
		get {
			return Current;}}
	public object Current {
		get {
			try {
				return fns[idx];}
			catch (System.IndexOutOfRangeException) {
				return false;}}}}