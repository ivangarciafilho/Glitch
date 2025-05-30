﻿using UnityEngine;

namespace FIMSpace
{
    [DefaultExecutionOrder(-18)]
    [HelpURL( "https://assetstore.unity.com/packages/tools/animation/leaning-animator-198472" )]
    [AddComponentMenu("FImpossible Creations/Leaning Animator")]
    public class LeaningAnimator : FimpossibleComponent
    {
        public enum ELeaningEditorCat { Setup, Leaning, Advanced }
        [HideInInspector] public ELeaningEditorCat _EditorDrawSetup = ELeaningEditorCat.Setup;

        [SerializeField]//[HideInInspector]
        private LeaningProcessor Leaning = new LeaningProcessor();

        public LeaningProcessor Parameters { get { return Leaning; } }

        /// <summary> Manually inform leaning that character is accelerating (turn off "TryAutoDetectAcceleration" fist) </summary>
        public bool SetIsAccelerating { set { Parameters.IsCharacterAccelerating = value; } }
        /// <summary> Manually inform leaning that character is on the ground </summary>
        public bool SetIsGrounded { set { Parameters.IsCharacterGrounded = value; } }
        public bool CheckIfIsGrounded { get { return Parameters.IsCharacterGrounded; } }
        public bool CheckIfIsAcccelerating { get { return Parameters.accelerating; } }

        public void Reset()
        {
            if( Leaning == null ) Leaning = new LeaningProcessor();
            Leaning.TryAutoFindReferences( transform );
        }

        public void Start()
        {
            if( Leaning == null ) Leaning = new LeaningProcessor();
            Leaning.Initialize( this );
        }

        private void Update()
        {
            Leaning.Update();
        }

        private void FixedUpdate()
        {
            Leaning.FixedUpdate();
        }

        private void LateUpdate()
        {
#if UNITY_EDITOR
#if ENABLE_LEGACY_INPUT_MANAGER
            if (Input.GetKey(KeyCode.BackQuote)) return; // Debug disable update
#endif
#endif

            Leaning.LateUpdate();
        }


#if UNITY_EDITOR
        private void OnDrawGizmosSelected()
        {
            Leaning.DrawGizmos();
        }

        public override void OnValidate()
        {
            base.OnValidate();
            if( Leaning != null ) Leaning.RefreshAnimiatorParams();
        }
#endif

        private void OnEnable()
        {
            if ( Leaning != null ) Leaning.OnEnable();

            // On recompile support
#if UNITY_EDITOR
#if UNITY_2021_3_OR_NEWER
            if (Leaning == null) Leaning = new LeaningProcessor();
            UnityEditor.AssemblyReloadEvents.afterAssemblyReload += () => Leaning.Initialize(this);
#endif
#endif
        }

        /// <summary> Restoring leaning properties after teleporting character to the new placement </summary>
        public void User_AfterTeleport()
        {
            StartCoroutine( Leaning.IEAfterTeleportReset() );
        }

        public void User_RotateSpineStart(Vector3 angles)
        {
            Leaning._UserStartBoneAddAngles = angles;
        }

        public void User_RotateSpineMiddle(Vector3 angles)
        {
            Leaning._UserMidBoneAddAngles = angles;
        }


        public void User_RotateOriginForward(float forwardAngle)
        {
            Leaning._UserOriginBoneAddForwardAngle = forwardAngle;
        }

        float _sd_origF = 0f;
        public void User_RotateOriginForward(float forwardAngle, float duration, float deltaTime)
        {
            float targetV = Mathf.SmoothDamp(Leaning._UserOriginBoneAddForwardAngle, forwardAngle, ref _sd_origF, duration, 1000000f, deltaTime);
            User_RotateOriginForward(targetV);
        }

        public void User_RotateOriginSide(float forwardAngle)
        {
            Leaning._UserOriginBoneAddSideAngle = forwardAngle;
        }

        float _sd_origS = 0f;
        public void User_RotateOriginSide(float sideAngle, float duration, float deltaTime)
        {
            float targetV = Mathf.SmoothDamp(Leaning._UserOriginBoneAddSideAngle, sideAngle, ref _sd_origS, duration, 1000000f, deltaTime);
            User_RotateOriginSide(targetV);
        }

        public void User_DeliverCustomRaycastHit(RaycastHit hit)
        {
            Leaning._UserUseCustomRaycast = true;
            Leaning._UserCustomRaycast = hit;
        }

        public void User_DeliverSlopeAngle(float angle)
        {
            Leaning._UserCustomSlopeAngle = angle;
        }

        public void User_DeliverAccelerationSpeed(float velocityMagnitude)
        {
            Leaning.customAccelerationVelocity = velocityMagnitude;
        }

        public void User_DeliverIsAccelerating(bool isAccelerating)
        {
            SetIsAccelerating = isAccelerating;
        }

        public void User_DeliverIsGrounded(bool grounded)
        {
            SetIsGrounded = grounded;
        }
    }
}