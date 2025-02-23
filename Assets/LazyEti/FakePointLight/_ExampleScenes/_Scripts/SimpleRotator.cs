using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace StylizedPointLight
{
    public class SimpleRotator : MonoBehaviour
    {
        [Range (-5, 5)]
        public float speed = 0.1f;
        public bool xAxis = false;
        public bool yAxis = false;
        public bool zAxis = false;


        // Start is called before the first frame update
        void Start()
        {

        }

        // Update is called once per frame
        void FixedUpdate()
        {
            transform.Rotate (speed * Convert.ToInt32 (xAxis), speed * Convert.ToInt32 (yAxis), speed * Convert.ToInt32 (zAxis));


        }
    }
}
