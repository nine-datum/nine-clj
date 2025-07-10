in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
uniform sampler2D texture2d;
uniform mat4 transform;
uniform mat4 projection;
uniform vec3 time;

void main (void)
{
  int ind = int(position.z);
  vec3 pos0 = (position - vec3(0.0, 0.0, position.z)) * vec3(0.02, 1.0, 0.02);
  vec3 pos = (transform * vec4(pos0, 1.0)).xyz;
  float sx = 100.0;
  float sz = 100.0;
  float hsx = sx * 0.5;
  float hsz = sz * 0.5;
  float wx = mod(float(ind), sx);
  float wz = float(int(float(ind) / sz));
  float h = texture(texture2d, vec2(wx / 100.0, wz / 100.0)).r;
  pos = pos + vec3(wx - hsx, 100.0 - mod(h * 500.0 + time.x * 50.0, 100.0), wz - hsz);
  
  uv = texcoord;
  worldNormal = normalize((transform * vec4(normal, 0.0)).xyz);
  gl_Position = projection * vec4(pos, 1.0);
}
