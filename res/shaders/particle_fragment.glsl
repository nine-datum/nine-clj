in vec2 uv;
in vec3 worldNormal;

out vec4 out_Color;

uniform sampler2D texture2d;
uniform vec3 worldLight;
uniform vec4 color;

void main (void)
{	
  vec4 c = texture(texture2d, uv);
  if(c.a < 0.5) { discard; }
	out_Color = vec4(color.rgb * c.rgb * (dot(worldNormal, -worldLight) + 1.0) * 0.5, 1.0);
}
