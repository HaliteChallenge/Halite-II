#version 400
uniform sampler2D textureSampler;
uniform vec3 color;
in vec2 UV;
out vec4 fragColor;
void main() {
    fragColor = vec4(texture(textureSampler, UV).rrr * color, 1);
}
