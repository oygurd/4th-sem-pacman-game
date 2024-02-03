Shader "Custom/NewSurfaceShader"
{
	Properties {
		texture_1 ("Texture 1", 2D) = "white" {}
		_MainTex ("Foo", 2D) = "white" {}
	}
	SubShader {
		Tags { "RenderType"="Opaque" }
		LOD 200
		CGPROGRAM
		#pragma surface surf Standard fullforwardshadows
		#pragma target 3.0
		
		sampler2D _MainTex;
		struct Input {
			float2 uv_MainTex;
		};
		UNITY_INSTANCING_BUFFER_START(Props)
		UNITY_INSTANCING_BUFFER_END(Props)
		
#define hlsl_atan(x,y) atan2(x, y)
#define mod(x,y) ((x)-(y)*floor((x)/(y)))
inline float4 textureLod(sampler2D tex, float2 uv, float lod) {
    return tex2D(tex, uv);
}
inline float2 tofloat2(float x) {
    return float2(x, x);
}
inline float2 tofloat2(float x, float y) {
    return float2(x, y);
}
inline float3 tofloat3(float x) {
    return float3(x, x, x);
}
inline float3 tofloat3(float x, float y, float z) {
    return float3(x, y, z);
}
inline float3 tofloat3(float2 xy, float z) {
    return float3(xy.x, xy.y, z);
}
inline float3 tofloat3(float x, float2 yz) {
    return float3(x, yz.x, yz.y);
}
inline float4 tofloat4(float x, float y, float z, float w) {
    return float4(x, y, z, w);
}
inline float4 tofloat4(float x) {
    return float4(x, x, x, x);
}
inline float4 tofloat4(float x, float3 yzw) {
    return float4(x, yzw.x, yzw.y, yzw.z);
}
inline float4 tofloat4(float2 xy, float2 zw) {
    return float4(xy.x, xy.y, zw.x, zw.y);
}
inline float4 tofloat4(float3 xyz, float w) {
    return float4(xyz.x, xyz.y, xyz.z, w);
}
inline float4 tofloat4(float2 xy, float z, float w) {
    return float4(xy.x, xy.y, z, w);
}
inline float2x2 tofloat2x2(float2 v1, float2 v2) {
    return float2x2(v1.x, v1.y, v2.x, v2.y);
}
// EngineSpecificDefinitions
float rand(float2 x) {
    return frac(cos(mod(dot(x, tofloat2(13.9898, 8.141)), 3.14)) * 43758.5453);
}
float2 rand2(float2 x) {
    return frac(cos(mod(tofloat2(dot(x, tofloat2(13.9898, 8.141)),
						      dot(x, tofloat2(3.4562, 17.398))), tofloat2(3.14))) * 43758.5453);
}
float3 rand3(float2 x) {
    return frac(cos(mod(tofloat3(dot(x, tofloat2(13.9898, 8.141)),
							  dot(x, tofloat2(3.4562, 17.398)),
                              dot(x, tofloat2(13.254, 5.867))), tofloat3(3.14))) * 43758.5453);
}
float param_rnd(float minimum, float maximum, float seed) {
	return minimum+(maximum-minimum)*rand(tofloat2(seed));
}
float3 rgb2hsv(float3 c) {
	float4 K = tofloat4(0.0, -1.0 / 3.0, 2.0 / 3.0, -1.0);
	float4 p = c.g < c.b ? tofloat4(c.bg, K.wz) : tofloat4(c.gb, K.xy);
	float4 q = c.r < p.x ? tofloat4(p.xyw, c.r) : tofloat4(c.r, p.yzx);
	float d = q.x - min(q.w, q.y);
	float e = 1.0e-10;
	return tofloat3(abs(q.z + (q.w - q.y) / (6.0 * d + e)), d / (q.x + e), q.x);
}
float3 hsv2rgb(float3 c) {
	float4 K = tofloat4(1.0, 2.0 / 3.0, 1.0 / 3.0, 3.0);
	float3 p = abs(frac(c.xxx + K.xyz) * 6.0 - K.www);
	return c.z * lerp(K.xxx, clamp(p - K.xxx, 0.0, 1.0), c.y);
}
// Based on https://www.shadertoy.com/view/ldl3W8
// The MIT License
// Copyright © 2013 Inigo Quilez
float3 iq_voronoi(float2 x, float2 size, float2 stretch, float randomness, float2 seed) {
	float2 n = floor(x);
	float2 f = frac(x);
	float2 mg, mr, mc;
	float md = 8.0;
	for (int j=-1; j<=1; j++)
	for (int i=-1; i<=1; i++) {
		float2 g = tofloat2(float(i),float(j));
		float2 o = randomness*rand2(seed + mod(n + g + size, size));
		float2 c = g + o;
		float2 r = c - f;
		float2 rr = r*stretch;
		float d = dot(rr,rr);
		if (d<md) {
			mc = c;
			md = d;
			mr = r;
			mg = g;
		}
	}
	md = 8.0;
	for (int j=-2; j<=2; j++)
	for (int i=-2; i<=2; i++) {
		float2 g = mg + tofloat2(float(i),float(j));
		float2 o = randomness*rand2(seed + mod(n + g + size, size));
		float2 r = g + o - f;
		float2 rr = (mr-r)*stretch;
		if (dot(rr,rr)>0.00001)
	   		md = min(md, dot(0.5*(mr+r)*stretch, normalize((r-mr)*stretch)));
	}
	return tofloat3(md, mc+n);
}
float4 voronoi(float2 uv, float2 size, float2 stretch, float intensity, float randomness, float seed) {
	uv *= size;
	float3 v = iq_voronoi(uv, size, stretch, randomness, rand2(tofloat2(seed, 1.0-seed)));
	return tofloat4(v.yz, intensity*length((uv-v.yz)*stretch), v.x);
}
float3 fill_to_uv_stretch(float2 coord, float4 bb, float seed) {
	float2 uv_islands = frac(coord-bb.xy)/bb.zw;
	float random_value = rand(tofloat2(seed)+bb.xy+bb.zw);
	return tofloat3(uv_islands, random_value);
}
float3 fill_to_uv_square(float2 coord, float4 bb, float seed) {
	float2 uv_islands;
	if (bb.z > bb.w) {
		float2 adjusted_coord = coord + tofloat2(0.0, (bb.z - bb.w) / 2.0);
		uv_islands = frac(adjusted_coord-bb.xy)/bb.zz;
	} else {
		float2 adjusted_coord = coord + tofloat2((bb.w - bb.z) / 2.0, 0.0);
		uv_islands = frac(adjusted_coord-bb.xy)/bb.ww;
	}
	float random_value = rand(tofloat2(seed)+bb.xy+bb.zw);
	return tofloat3(uv_islands, random_value);
}
float4 adjust_levels(float4 input, float4 in_min, float4 in_mid, float4 in_max, float4 out_min, float4 out_max) {
	input = clamp((input-in_min)/(in_max-in_min), 0.0, 1.0);
	in_mid = (in_mid-in_min)/(in_max-in_min);
	float4 dark = step(in_mid, input);
	input = 0.5*lerp(input/(in_mid), 1.0+(input-in_mid)/(1.0-in_mid), dark);
	return out_min+input*(out_max-out_min);
}
uniform sampler2D texture_1;
static const float texture_1_size = 1024.0;
const float pack_size = 256.0;
float2 pack_1x32_to_2x16(float s) {
	return tofloat2(s - mod(s, 1.0/pack_size), mod(s, 1.0/pack_size)*pack_size);
}
float pack_2x16_to_1x32(float2 s) {
	return s.x + s.y/pack_size;
}
float4 pack_2x32_to_4x16(float2 s) {
	return tofloat4(s.xy - tofloat2(mod(s.x, 1.0/pack_size), mod(s.y, 1.0/pack_size)) , tofloat2(mod(s.x, 1.0/pack_size), mod(s.y, 1.0/pack_size))*pack_size);
}
float2 pack_4x16_to_2x32(float4 s) {
	return s.rg + s.ba/pack_size;
}
static const float4 p_o14339_albedo_color = tofloat4(1.000000000, 1.000000000, 1.000000000, 1.000000000);
static const float p_o14339_metallic = 0.000000000;
static const float p_o14339_roughness = 0.150000006;
static const float p_o14339_emission_energy = 1.000000000;
static const float p_o14339_normal = 1.000000000;
static const float p_o14339_ao = 1.000000000;
static const float p_o14339_depth_scale = 0.800000000;
static const float p_o501924_gradient_0_pos = 0.236363636;
static const float4 p_o501924_gradient_0_col = tofloat4(0.000000000, 0.000000000, 0.000000000, 1.000000000);
static const float p_o501924_gradient_1_pos = 0.481818182;
static const float4 p_o501924_gradient_1_col = tofloat4(1.000000000, 1.000000000, 1.000000000, 1.000000000);
float4 o501924_gradient_gradient_fct(float x) {
  if (x < p_o501924_gradient_0_pos) {
    return p_o501924_gradient_0_col;
  } else if (x < p_o501924_gradient_1_pos) {
    return lerp(p_o501924_gradient_0_col, p_o501924_gradient_1_col, 0.5-0.5*cos(3.14159265359*(x-p_o501924_gradient_0_pos)/(p_o501924_gradient_1_pos-p_o501924_gradient_0_pos)));
  }
  return p_o501924_gradient_1_col;
}
float o14339_input_depth_tex(float2 uv, float _seed_variation_) {
float4 o501924_0_1_rgba = o501924_gradient_gradient_fct(((uv)).x);
return (dot((o501924_0_1_rgba).rgb, tofloat3(1.0))/3.0);
}
static const float p_o87559_gradient_0_pos = 0.000000000;
static const float4 p_o87559_gradient_0_col = tofloat4(0.419769287, 0.529251158, 0.820312500, 1.000000000);
static const float p_o87559_gradient_1_pos = 1.000000000;
static const float4 p_o87559_gradient_1_col = tofloat4(0.022827148, 0.030771255, 0.531250000, 1.000000000);
float4 o87559_gradient_gradient_fct(float x) {
  if (x < p_o87559_gradient_0_pos) {
    return p_o87559_gradient_0_col;
  } else if (x < p_o87559_gradient_1_pos) {
    return lerp(p_o87559_gradient_0_col, p_o87559_gradient_1_col, (x-p_o87559_gradient_0_pos)/(p_o87559_gradient_1_pos-p_o87559_gradient_0_pos));
  }
  return p_o87559_gradient_1_col;
}
static const float4 p_o68764_in_min = tofloat4(0.000000000, 0.000000000, 0.000000000, 0.000000000);
static const float4 p_o68764_in_mid = tofloat4(0.533333361, 0.533333361, 0.533333361, 0.500000000);
static const float4 p_o68764_in_max = tofloat4(1.000000000, 1.000000000, 1.000000000, 1.000000000);
static const float4 p_o68764_out_min = tofloat4(0.008680558, 0.008680558, 0.008680558, 0.000000000);
static const float4 p_o68764_out_max = tofloat4(0.965277851, 0.965277851, 0.965277851, 1.000000000);
static const float seed_o34580 = 0.000000000;
static const float seed_o23463 = 0.086306274;
static const float p_o23463_scale_x = 4.000000000;
static const float p_o23463_scale_y = 4.000000000;
static const float p_o23463_stretch_x = 0.970000000;
static const float p_o23463_stretch_y = 1.430000000;
static const float p_o23463_intensity = 0.240000000;
static const float p_o23463_randomness = 0.710000000;
static const float p_o41667_amount = 1.000000000;
float3 o41672_input_source(float2 uv, float _seed_variation_) {
float4 o41661_0 = textureLod(texture_1, uv, 0.0);
return ((o41661_0).rgb);
}
float o41667_input_in(float2 uv, float _seed_variation_) {
float o41672_0_1_f = pack_2x16_to_1x32(o41672_input_source((uv), _seed_variation_).xy);
return o41672_0_1_f;
}
float3 nm_o41667(float2 uv, float amount, float size, float _seed_variation_) {
	float3 e = tofloat3(1.0/size, -1.0/size, 0);
	float2 rv;
	if (0 == 0) {
		rv = tofloat2(1.0, -1.0)*o41667_input_in(uv+e.xy, _seed_variation_);
		rv += tofloat2(-1.0, 1.0)*o41667_input_in(uv-e.xy, _seed_variation_);
		rv += tofloat2(1.0, 1.0)*o41667_input_in(uv+e.xx, _seed_variation_);
		rv += tofloat2(-1.0, -1.0)*o41667_input_in(uv-e.xx, _seed_variation_);
		rv += tofloat2(2.0, 0.0)*o41667_input_in(uv+e.xz, _seed_variation_);
		rv += tofloat2(-2.0, 0.0)*o41667_input_in(uv-e.xz, _seed_variation_);
		rv += tofloat2(0.0, 2.0)*o41667_input_in(uv+e.zx, _seed_variation_);
		rv += tofloat2(0.0, -2.0)*o41667_input_in(uv-e.zx, _seed_variation_);
		rv *= size*amount/128.0;
	} else if (0 == 1) {
		rv = tofloat2(3.0, -3.0)*o41667_input_in(uv+e.xy, _seed_variation_);
		rv += tofloat2(-3.0, 3.0)*o41667_input_in(uv-e.xy, _seed_variation_);
		rv += tofloat2(3.0, 3.0)*o41667_input_in(uv+e.xx, _seed_variation_);
		rv += tofloat2(-3.0, -3.0)*o41667_input_in(uv-e.xx, _seed_variation_);
		rv += tofloat2(10.0, 0.0)*o41667_input_in(uv+e.xz, _seed_variation_);
		rv += tofloat2(-10.0, 0.0)*o41667_input_in(uv-e.xz, _seed_variation_);
		rv += tofloat2(0.0, 10.0)*o41667_input_in(uv+e.zx, _seed_variation_);
		rv += tofloat2(0.0, -10.0)*o41667_input_in(uv-e.zx, _seed_variation_);
		rv *= size*amount/512.0;
	} else if (0 == 2) {
		rv = tofloat2(2.0, 0.0)*o41667_input_in(uv+e.xz, _seed_variation_);
		rv += tofloat2(-2.0, 0.0)*o41667_input_in(uv-e.xz, _seed_variation_);
		rv += tofloat2(0.0, 2.0)*o41667_input_in(uv+e.zx, _seed_variation_);
		rv += tofloat2(0.0, -2.0)*o41667_input_in(uv-e.zx, _seed_variation_);
		rv *= size*amount/64.0;
	} else {
		rv = tofloat2(1.0, 0.0)*o41667_input_in(uv+e.xz, _seed_variation_);
		rv += tofloat2(0.0, 1.0)*o41667_input_in(uv+e.zx, _seed_variation_);
		rv += tofloat2(-1.0, -1.0)*o41667_input_in(uv, _seed_variation_);
		rv *= size*amount/20.0;
	}
	return tofloat3(0.5)+0.5*normalize(tofloat3(rv, -1.0));
}
		
		void surf (Input IN, inout SurfaceOutputStandard o) {
	  		float _seed_variation_ = 0.0;
			float2 uv = IN.uv_MainTex;
float4 o23463_0_xyzw = voronoi((uv), tofloat2(p_o23463_scale_x, p_o23463_scale_y), tofloat2(p_o23463_stretch_y, p_o23463_stretch_x), p_o23463_intensity, p_o23463_randomness, (seed_o23463+frac(_seed_variation_)));float4 o23463_2_1_fill = round(tofloat4(frac((o23463_0_xyzw.xy-1.0)/tofloat2(p_o23463_scale_x, p_o23463_scale_y)), tofloat2(2.0)/tofloat2(p_o23463_scale_x, p_o23463_scale_y))*4096.0)/4096.0;
float4 o34580_0_bb = o23463_2_1_fill;float3 o34580_0_1_rgb = fill_to_uv_stretch((uv), o34580_0_bb, float((seed_o34580+frac(_seed_variation_))));
float4 o68764_0_1_rgba = adjust_levels(tofloat4(o34580_0_1_rgb, 1.0), p_o68764_in_min, p_o68764_in_mid, p_o68764_in_max, p_o68764_out_min, p_o68764_out_max);
float4 o87559_0_1_rgba = o87559_gradient_gradient_fct((dot((o68764_0_1_rgba).rgb, tofloat3(1.0))/3.0));
float3 o41667_0_1_rgb = nm_o41667((uv), p_o41667_amount, 1024.000000000, _seed_variation_);

			o.Albedo = ((o87559_0_1_rgba).rgb).rgb*p_o14339_albedo_color.rgb;
			o.Metallic = 1.0*p_o14339_metallic;
			o.Smoothness = 1.0-1.0*p_o14339_roughness;
			o.Alpha = 1.0;
			o.Normal = o41667_0_1_rgb*tofloat3(-1.0, 1.0, -1.0)+tofloat3(1.0, 0.0, 1.0);

		}
		ENDCG
	}
	FallBack "Diffuse"
}



