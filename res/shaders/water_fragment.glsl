in vec2 uv;
in vec3 worldNormal;
in vec3 worldPos;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;
uniform vec4 color;
uniform vec3 time;

void main (void)
{
	out_Color = vec4(color.rgb * texture(texture2d, worldPos.xz * 0.01).rgb * (dot(worldNormal, -worldLight) + 1.0) * 0.5, 1.0);
}
