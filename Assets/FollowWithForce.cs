using UnityEngine;

public class FollowWithForce : MonoBehaviour
{

    public Rigidbody itsRigidbody;
    public Transform[] forcePoints;
    public Transform[] forceTargets;
    public Transform[] gravityPoints;
    public float trackingForce = 1.0f;
    public float gravityStrength;


    private void FixedUpdate()
    {

        float smoothing = Time.fixedDeltaTime;
        var iterations = Mathf.Min(forcePoints.Length, forceTargets.Length);
        for (int i = 0; i < iterations; i++)
        {
            var forcePointPosition =  forcePoints[i].position;
            var forceTargetPosition = forceTargets[i].position;
            var directionTo =  forceTargetPosition - forcePointPosition ;

            itsRigidbody.AddForceAtPosition(directionTo * ( smoothing * trackingForce ), forcePointPosition,ForceMode.VelocityChange);
        }

        iterations = gravityPoints.Length;
            for (int i = 0;i < iterations; i++)
        {
            itsRigidbody.AddForceAtPosition(Physics.gravity*(gravityStrength*smoothing), forcePoints[i].position, ForceMode.VelocityChange);
        }
    }
}
