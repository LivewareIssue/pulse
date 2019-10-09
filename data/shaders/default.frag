#version 410 core

in vec3 f_position;
in vec2 f_uv;

out vec4 fragColor;

uniform sampler2D diffuse;

/*
    Samples the given (diffuse) texture at the given UV coordinates to determine
    the fragment color.
*/
void main(void)
{
    /*
    vec3 dx = dFdx(f_position);
    vec3 dy = dFdy(f_position);
    vec3 n  = normalize(cross(dx, dy));
    */

    vec4 diffuse_color = texture(diffuse, f_uv);

    fragColor = diffuse_color;
}
