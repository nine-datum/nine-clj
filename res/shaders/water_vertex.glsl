in vec3 position;
in vec2 texcoord;
in vec3 normal;

out vec2 uv;
out vec3 worldNormal;
out vec3 worldPos;
uniform mat4 transform;
uniform mat4 projection;
uniform vec3 time;

void main (void)
{
	uv = texcoord;
	vec3 pos = (transform * vec4(position, 1.0)).xyz;
	vec3 offset = vec3(0.0, sin(time.x + pos.x * 0.25) + 1.0, 0.0);
	worldNormal = normalize((transform * vec4(normal, 0.0)).xyz);
	worldPos = pos + offset;
	gl_Position = projection * vec4(worldPos, 1.0);
}
