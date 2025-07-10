in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
uniform mat4 transform;
uniform mat4 projection;
uniform vec3 time;

vec3 rotY (vec3 v, float angle)
{
  float s = sin(angle);
  float c = cos(angle);

  return mat3(
    c, 0.0, -s,
    0.0, 1.0, 0.0,
    s, 0.0, c
  ) * v;
}

vec3 rotX (vec3 v, float angle)
{
  float s = sin(angle);
  float c = cos(angle);

  return mat3(
    1.0, 0.0, 0.0,
    0.0, c, s,
    0.0, -s, c
  ) * v;
}

void main (void)
{
  int ind = int(position.z);
  float ang = float(ind) * 3.14 * 0.25;
  
  vec3 pos0 = position - vec3(0, 0, position.z);
  pos0 = pos0 * 0.5;
  pos0 = rotX(pos0, 3.14 * 0.15);
  pos0 = rotY(pos0, ang);
  vec3 pos = (transform * vec4(pos0, 1)).xyz;

  float s = 2.0;
  float nx = sin(ang) * s;
  float nz = cos(ang) * s;
  float g = 4.0;
  pos = pos
    + vec3(0, 1.0 + float(int(ind / 8)) * 0.25, 0)
    + vec3(nx, 2, nz) * time.x
    - vec3(0, 1, 0) * (g * time.x * time.x * 0.5);
  
  uv = texcoord;
  worldNormal = normalize((transform * vec4(normal, 0)).xyz);
  gl_Position = projection * vec4(pos, 1);
}
