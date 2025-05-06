using UnityEngine;

// !! IMPORTANT !!
// Make sure your Animator component is set to Update Mode -> Animate Physics

public class SetAnimatorFramerate : MonoBehaviour
{
    public Animator _animator;
    public int FPS = 8;
    private float _time;
    public Vector2 noisySpeedRange;
    public float noisySpeed;

    private void Update()
    {
        noisySpeed = Mathf.PerlinNoise1D( Time.time );
        _animator.speed = Mathf.Lerp( noisySpeedRange.x, noisySpeedRange.y, noisySpeed );
    }


    void FixedUpdate()
    {
        _time += Time.fixedDeltaTime;
        var updateTime = 1f / FPS;
        _animator.speed = 0;

        if (_time > updateTime)
        {
            _time -= updateTime;
            _animator.speed = 60f / FPS;
        }
    }
}
