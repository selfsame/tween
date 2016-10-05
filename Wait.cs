using UnityEngine;
using System.Collections;

public class Wait{
	public   bool  active = false;
	public  float duration;
	public  float end;
	public Wait (float _duration){
		duration = _duration;	
	}
	public bool invoke(){
		if (!active){
			active = true;
			end = UnityEngine.Time.time + duration;}
		if (end < UnityEngine.Time.time ){
			active = false;
			return false;}
		return true;
	}

	public float ratio{
		get{
			if (!active){
				return 0f;
			}
		  	return 1f - (end - UnityEngine.Time.time) /  duration;
		}
	}
}