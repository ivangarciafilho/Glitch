using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FollowReference : MonoBehaviour
{
    public Transform reference;

    public void Update()
    {
        transform.SetPositionAndRotation(reference.position, reference.rotation);
    }

    public void FixedUpdate()
    {
        transform.SetPositionAndRotation(reference.position, reference.rotation);
    }

}
