using System.Collections;
using System.Collections.Generic;
using UnityEngine;

namespace StylizedPointLight
{

    public class RandomFlicker : MonoBehaviour
    {
        private MeshRenderer[] meshes;
        //private List< MeshRenderer> meshes = new List<MeshRenderer>();
        private MaterialPropertyBlock propertyBlock;
        int randomID = Shader.PropertyToID ("_RandomOffset");
        [SerializeField] bool refresh;

        private void Awake()
        {
            propertyBlock = new MaterialPropertyBlock ();
        }

        private void Update()
        {
            if (refresh)
            {
                refresh = false;
                SetProperties ();
            }
        }

        void SetProperties()
        {
            meshes = GetComponentsInChildren<MeshRenderer> ();


            foreach (MeshRenderer m in meshes)
            {
                if (m == null) continue;

                //optimized way:
                propertyBlock.SetFloat (randomID, Random.value);
                m.SetPropertyBlock (propertyBlock);

                //simple way:
                //mesh.material.SetFloat ("_RandomOffset", Random.value);
            }
        }


    }
}
