using System.Runtime.CompilerServices;

using UnityEngine;

public class SpeedTracker : MonoBehaviour
{
    public float speedCurrent;
    public float speedPrevious;
    public Vector3 positionCurrent;
    public Vector3 positionPrevious;

    private void LateUpdate()
    {
        positionCurrent = transform.position;


        speedCurrent = Mathf.Lerp(speedPrevious,  Vector3.Distance(positionPrevious,positionCurrent) * Time.deltaTime, 0.666f ); 

        speedPrevious = speedCurrent;
        positionCurrent = positionPrevious;
    }
}
