
<!doctype html public "-//W3C//DTD HTML 3.2//EN">
<html>

<head>
  <title>C++/src/app/blast/update_blastdb.pl</title>
  <base href="//www.ncbi.nlm.nih.gov/IEB/ToolBox/CPP_DOC/lxr/">
  <link rel="stylesheet" type="text/css" href="//www.ncbi.nlm.nih.gov/corehtml/ncbi.css">
  <link rel="stylesheet" type="text/css" href="lxr.css">
  <script type="text/javascript" src="lxr.js"></script>
</head>

<body bgcolor=white>

<table border=0 width="100%">
<tr>
  <td class=menu align=left nowrap>
    <a href="/">NCBI Home</a><br>
    <a href="/IEB/">IEB Home</a><br>
    <a href="//ncbi.github.io/cxx-toolkit/">C++ Toolkit docs</a><br>
    <a href="/IEB/ToolBox/C_DOC/lxr/source">C Toolkit source browser</a><br>
    <a href="//www.ncbi.nlm.nih.gov/IEB/ToolBox/SB/hbr.html">C Toolkit source browser (2)</a><br>
  </td>
  <td width="55%" align="center">
    <h1>NCBI C++ Toolkit Cross Reference</h1>
    <h3><script type="text/javascript">SVNLocations();</script>&nbsp; <a href="source/">C++</a>/<a href="source/src/">src</a>/<a href="source/src/app/">app</a>/<a href="source/src/app/blast/">blast</a>/<a href="source/src/app/blast/update_blastdb.pl">update_blastdb.pl</a></h3>
  </td>
  <td class=menu align=right nowrap>
      <b><i>source navigation</i></b><br>  <a href="diff/src/app/blast/update_blastdb.pl">diff markup</a><br>  <a href="ident">identifier search</a><br>  <a href="search">freetext search</a><br>  <a href="find">file search</a><br> 
  </td>
</tr>
</table>

<hr>
<pre><div class="src-wrapper"><div class="src-numbers"><a name=L1 href="source/src/app/blast/update_blastdb.pl#L1">  1</a>
<a name=L2 href="source/src/app/blast/update_blastdb.pl#L2">  2</a>
<a name=L3 href="source/src/app/blast/update_blastdb.pl#L3">  3</a>
<a name=L4 href="source/src/app/blast/update_blastdb.pl#L4">  4</a>
<a name=L5 href="source/src/app/blast/update_blastdb.pl#L5">  5</a>
<a name=L6 href="source/src/app/blast/update_blastdb.pl#L6">  6</a>
<a name=L7 href="source/src/app/blast/update_blastdb.pl#L7">  7</a>
<a name=L8 href="source/src/app/blast/update_blastdb.pl#L8">  8</a>
<a name=L9 href="source/src/app/blast/update_blastdb.pl#L9">  9</a>
<a name=L10 href="source/src/app/blast/update_blastdb.pl#L10"> 10</a>
<a name=L11 href="source/src/app/blast/update_blastdb.pl#L11"> 11</a>
<a name=L12 href="source/src/app/blast/update_blastdb.pl#L12"> 12</a>
<a name=L13 href="source/src/app/blast/update_blastdb.pl#L13"> 13</a>
<a name=L14 href="source/src/app/blast/update_blastdb.pl#L14"> 14</a>
<a name=L15 href="source/src/app/blast/update_blastdb.pl#L15"> 15</a>
<a name=L16 href="source/src/app/blast/update_blastdb.pl#L16"> 16</a>
<a name=L17 href="source/src/app/blast/update_blastdb.pl#L17"> 17</a>
<a name=L18 href="source/src/app/blast/update_blastdb.pl#L18"> 18</a>
<a name=L19 href="source/src/app/blast/update_blastdb.pl#L19"> 19</a>
<a name=L20 href="source/src/app/blast/update_blastdb.pl#L20"> 20</a>
<a name=L21 href="source/src/app/blast/update_blastdb.pl#L21"> 21</a>
<a name=L22 href="source/src/app/blast/update_blastdb.pl#L22"> 22</a>
<a name=L23 href="source/src/app/blast/update_blastdb.pl#L23"> 23</a>
<a name=L24 href="source/src/app/blast/update_blastdb.pl#L24"> 24</a>
<a name=L25 href="source/src/app/blast/update_blastdb.pl#L25"> 25</a>
<a name=L26 href="source/src/app/blast/update_blastdb.pl#L26"> 26</a>
<a name=L27 href="source/src/app/blast/update_blastdb.pl#L27"> 27</a>
<a name=L28 href="source/src/app/blast/update_blastdb.pl#L28"> 28</a>
<a name=L29 href="source/src/app/blast/update_blastdb.pl#L29"> 29</a>
<a name=L30 href="source/src/app/blast/update_blastdb.pl#L30"> 30</a>
<a name=L31 href="source/src/app/blast/update_blastdb.pl#L31"> 31</a>
<a name=L32 href="source/src/app/blast/update_blastdb.pl#L32"> 32</a>
<a name=L33 href="source/src/app/blast/update_blastdb.pl#L33"> 33</a>
<a name=L34 href="source/src/app/blast/update_blastdb.pl#L34"> 34</a>
<a name=L35 href="source/src/app/blast/update_blastdb.pl#L35"> 35</a>
<a name=L36 href="source/src/app/blast/update_blastdb.pl#L36"> 36</a>
<a name=L37 href="source/src/app/blast/update_blastdb.pl#L37"> 37</a>
<a name=L38 href="source/src/app/blast/update_blastdb.pl#L38"> 38</a>
<a name=L39 href="source/src/app/blast/update_blastdb.pl#L39"> 39</a>
<a name=L40 href="source/src/app/blast/update_blastdb.pl#L40"> 40</a>
<a name=L41 href="source/src/app/blast/update_blastdb.pl#L41"> 41</a>
<a name=L42 href="source/src/app/blast/update_blastdb.pl#L42"> 42</a>
<a name=L43 href="source/src/app/blast/update_blastdb.pl#L43"> 43</a>
<a name=L44 href="source/src/app/blast/update_blastdb.pl#L44"> 44</a>
<a name=L45 href="source/src/app/blast/update_blastdb.pl#L45"> 45</a>
<a name=L46 href="source/src/app/blast/update_blastdb.pl#L46"> 46</a>
<a name=L47 href="source/src/app/blast/update_blastdb.pl#L47"> 47</a>
<a name=L48 href="source/src/app/blast/update_blastdb.pl#L48"> 48</a>
<a name=L49 href="source/src/app/blast/update_blastdb.pl#L49"> 49</a>
<a name=L50 href="source/src/app/blast/update_blastdb.pl#L50"> 50</a>
<a name=L51 href="source/src/app/blast/update_blastdb.pl#L51"> 51</a>
<a name=L52 href="source/src/app/blast/update_blastdb.pl#L52"> 52</a>
<a name=L53 href="source/src/app/blast/update_blastdb.pl#L53"> 53</a>
<a name=L54 href="source/src/app/blast/update_blastdb.pl#L54"> 54</a>
<a name=L55 href="source/src/app/blast/update_blastdb.pl#L55"> 55</a>
<a name=L56 href="source/src/app/blast/update_blastdb.pl#L56"> 56</a>
<a name=L57 href="source/src/app/blast/update_blastdb.pl#L57"> 57</a>
<a name=L58 href="source/src/app/blast/update_blastdb.pl#L58"> 58</a>
<a name=L59 href="source/src/app/blast/update_blastdb.pl#L59"> 59</a>
<a name=L60 href="source/src/app/blast/update_blastdb.pl#L60"> 60</a>
<a name=L61 href="source/src/app/blast/update_blastdb.pl#L61"> 61</a>
<a name=L62 href="source/src/app/blast/update_blastdb.pl#L62"> 62</a>
<a name=L63 href="source/src/app/blast/update_blastdb.pl#L63"> 63</a>
<a name=L64 href="source/src/app/blast/update_blastdb.pl#L64"> 64</a>
<a name=L65 href="source/src/app/blast/update_blastdb.pl#L65"> 65</a>
<a name=L66 href="source/src/app/blast/update_blastdb.pl#L66"> 66</a>
<a name=L67 href="source/src/app/blast/update_blastdb.pl#L67"> 67</a>
<a name=L68 href="source/src/app/blast/update_blastdb.pl#L68"> 68</a>
<a name=L69 href="source/src/app/blast/update_blastdb.pl#L69"> 69</a>
<a name=L70 href="source/src/app/blast/update_blastdb.pl#L70"> 70</a>
<a name=L71 href="source/src/app/blast/update_blastdb.pl#L71"> 71</a>
<a name=L72 href="source/src/app/blast/update_blastdb.pl#L72"> 72</a>
<a name=L73 href="source/src/app/blast/update_blastdb.pl#L73"> 73</a>
<a name=L74 href="source/src/app/blast/update_blastdb.pl#L74"> 74</a>
<a name=L75 href="source/src/app/blast/update_blastdb.pl#L75"> 75</a>
<a name=L76 href="source/src/app/blast/update_blastdb.pl#L76"> 76</a>
<a name=L77 href="source/src/app/blast/update_blastdb.pl#L77"> 77</a>
<a name=L78 href="source/src/app/blast/update_blastdb.pl#L78"> 78</a>
<a name=L79 href="source/src/app/blast/update_blastdb.pl#L79"> 79</a>
<a name=L80 href="source/src/app/blast/update_blastdb.pl#L80"> 80</a>
<a name=L81 href="source/src/app/blast/update_blastdb.pl#L81"> 81</a>
<a name=L82 href="source/src/app/blast/update_blastdb.pl#L82"> 82</a>
<a name=L83 href="source/src/app/blast/update_blastdb.pl#L83"> 83</a>
<a name=L84 href="source/src/app/blast/update_blastdb.pl#L84"> 84</a>
<a name=L85 href="source/src/app/blast/update_blastdb.pl#L85"> 85</a>
<a name=L86 href="source/src/app/blast/update_blastdb.pl#L86"> 86</a>
<a name=L87 href="source/src/app/blast/update_blastdb.pl#L87"> 87</a>
<a name=L88 href="source/src/app/blast/update_blastdb.pl#L88"> 88</a>
<a name=L89 href="source/src/app/blast/update_blastdb.pl#L89"> 89</a>
<a name=L90 href="source/src/app/blast/update_blastdb.pl#L90"> 90</a>
<a name=L91 href="source/src/app/blast/update_blastdb.pl#L91"> 91</a>
<a name=L92 href="source/src/app/blast/update_blastdb.pl#L92"> 92</a>
<a name=L93 href="source/src/app/blast/update_blastdb.pl#L93"> 93</a>
<a name=L94 href="source/src/app/blast/update_blastdb.pl#L94"> 94</a>
<a name=L95 href="source/src/app/blast/update_blastdb.pl#L95"> 95</a>
<a name=L96 href="source/src/app/blast/update_blastdb.pl#L96"> 96</a>
<a name=L97 href="source/src/app/blast/update_blastdb.pl#L97"> 97</a>
<a name=L98 href="source/src/app/blast/update_blastdb.pl#L98"> 98</a>
<a name=L99 href="source/src/app/blast/update_blastdb.pl#L99"> 99</a>
<a name=L100 href="source/src/app/blast/update_blastdb.pl#L100">100</a>
<a name=L101 href="source/src/app/blast/update_blastdb.pl#L101">101</a>
<a name=L102 href="source/src/app/blast/update_blastdb.pl#L102">102</a>
<a name=L103 href="source/src/app/blast/update_blastdb.pl#L103">103</a>
<a name=L104 href="source/src/app/blast/update_blastdb.pl#L104">104</a>
<a name=L105 href="source/src/app/blast/update_blastdb.pl#L105">105</a>
<a name=L106 href="source/src/app/blast/update_blastdb.pl#L106">106</a>
<a name=L107 href="source/src/app/blast/update_blastdb.pl#L107">107</a>
<a name=L108 href="source/src/app/blast/update_blastdb.pl#L108">108</a>
<a name=L109 href="source/src/app/blast/update_blastdb.pl#L109">109</a>
<a name=L110 href="source/src/app/blast/update_blastdb.pl#L110">110</a>
<a name=L111 href="source/src/app/blast/update_blastdb.pl#L111">111</a>
<a name=L112 href="source/src/app/blast/update_blastdb.pl#L112">112</a>
<a name=L113 href="source/src/app/blast/update_blastdb.pl#L113">113</a>
<a name=L114 href="source/src/app/blast/update_blastdb.pl#L114">114</a>
<a name=L115 href="source/src/app/blast/update_blastdb.pl#L115">115</a>
<a name=L116 href="source/src/app/blast/update_blastdb.pl#L116">116</a>
<a name=L117 href="source/src/app/blast/update_blastdb.pl#L117">117</a>
<a name=L118 href="source/src/app/blast/update_blastdb.pl#L118">118</a>
<a name=L119 href="source/src/app/blast/update_blastdb.pl#L119">119</a>
<a name=L120 href="source/src/app/blast/update_blastdb.pl#L120">120</a>
<a name=L121 href="source/src/app/blast/update_blastdb.pl#L121">121</a>
<a name=L122 href="source/src/app/blast/update_blastdb.pl#L122">122</a>
<a name=L123 href="source/src/app/blast/update_blastdb.pl#L123">123</a>
<a name=L124 href="source/src/app/blast/update_blastdb.pl#L124">124</a>
<a name=L125 href="source/src/app/blast/update_blastdb.pl#L125">125</a>
<a name=L126 href="source/src/app/blast/update_blastdb.pl#L126">126</a>
<a name=L127 href="source/src/app/blast/update_blastdb.pl#L127">127</a>
<a name=L128 href="source/src/app/blast/update_blastdb.pl#L128">128</a>
<a name=L129 href="source/src/app/blast/update_blastdb.pl#L129">129</a>
<a name=L130 href="source/src/app/blast/update_blastdb.pl#L130">130</a>
<a name=L131 href="source/src/app/blast/update_blastdb.pl#L131">131</a>
<a name=L132 href="source/src/app/blast/update_blastdb.pl#L132">132</a>
<a name=L133 href="source/src/app/blast/update_blastdb.pl#L133">133</a>
<a name=L134 href="source/src/app/blast/update_blastdb.pl#L134">134</a>
<a name=L135 href="source/src/app/blast/update_blastdb.pl#L135">135</a>
<a name=L136 href="source/src/app/blast/update_blastdb.pl#L136">136</a>
<a name=L137 href="source/src/app/blast/update_blastdb.pl#L137">137</a>
<a name=L138 href="source/src/app/blast/update_blastdb.pl#L138">138</a>
<a name=L139 href="source/src/app/blast/update_blastdb.pl#L139">139</a>
<a name=L140 href="source/src/app/blast/update_blastdb.pl#L140">140</a>
<a name=L141 href="source/src/app/blast/update_blastdb.pl#L141">141</a>
<a name=L142 href="source/src/app/blast/update_blastdb.pl#L142">142</a>
<a name=L143 href="source/src/app/blast/update_blastdb.pl#L143">143</a>
<a name=L144 href="source/src/app/blast/update_blastdb.pl#L144">144</a>
<a name=L145 href="source/src/app/blast/update_blastdb.pl#L145">145</a>
<a name=L146 href="source/src/app/blast/update_blastdb.pl#L146">146</a>
<a name=L147 href="source/src/app/blast/update_blastdb.pl#L147">147</a>
<a name=L148 href="source/src/app/blast/update_blastdb.pl#L148">148</a>
<a name=L149 href="source/src/app/blast/update_blastdb.pl#L149">149</a>
<a name=L150 href="source/src/app/blast/update_blastdb.pl#L150">150</a>
<a name=L151 href="source/src/app/blast/update_blastdb.pl#L151">151</a>
<a name=L152 href="source/src/app/blast/update_blastdb.pl#L152">152</a>
<a name=L153 href="source/src/app/blast/update_blastdb.pl#L153">153</a>
<a name=L154 href="source/src/app/blast/update_blastdb.pl#L154">154</a>
<a name=L155 href="source/src/app/blast/update_blastdb.pl#L155">155</a>
<a name=L156 href="source/src/app/blast/update_blastdb.pl#L156">156</a>
<a name=L157 href="source/src/app/blast/update_blastdb.pl#L157">157</a>
<a name=L158 href="source/src/app/blast/update_blastdb.pl#L158">158</a>
<a name=L159 href="source/src/app/blast/update_blastdb.pl#L159">159</a>
<a name=L160 href="source/src/app/blast/update_blastdb.pl#L160">160</a>
<a name=L161 href="source/src/app/blast/update_blastdb.pl#L161">161</a>
<a name=L162 href="source/src/app/blast/update_blastdb.pl#L162">162</a>
<a name=L163 href="source/src/app/blast/update_blastdb.pl#L163">163</a>
<a name=L164 href="source/src/app/blast/update_blastdb.pl#L164">164</a>
<a name=L165 href="source/src/app/blast/update_blastdb.pl#L165">165</a>
<a name=L166 href="source/src/app/blast/update_blastdb.pl#L166">166</a>
<a name=L167 href="source/src/app/blast/update_blastdb.pl#L167">167</a>
<a name=L168 href="source/src/app/blast/update_blastdb.pl#L168">168</a>
<a name=L169 href="source/src/app/blast/update_blastdb.pl#L169">169</a>
<a name=L170 href="source/src/app/blast/update_blastdb.pl#L170">170</a>
<a name=L171 href="source/src/app/blast/update_blastdb.pl#L171">171</a>
<a name=L172 href="source/src/app/blast/update_blastdb.pl#L172">172</a>
<a name=L173 href="source/src/app/blast/update_blastdb.pl#L173">173</a>
<a name=L174 href="source/src/app/blast/update_blastdb.pl#L174">174</a>
<a name=L175 href="source/src/app/blast/update_blastdb.pl#L175">175</a>
<a name=L176 href="source/src/app/blast/update_blastdb.pl#L176">176</a>
<a name=L177 href="source/src/app/blast/update_blastdb.pl#L177">177</a>
<a name=L178 href="source/src/app/blast/update_blastdb.pl#L178">178</a>
<a name=L179 href="source/src/app/blast/update_blastdb.pl#L179">179</a>
<a name=L180 href="source/src/app/blast/update_blastdb.pl#L180">180</a>
<a name=L181 href="source/src/app/blast/update_blastdb.pl#L181">181</a>
<a name=L182 href="source/src/app/blast/update_blastdb.pl#L182">182</a>
<a name=L183 href="source/src/app/blast/update_blastdb.pl#L183">183</a>
<a name=L184 href="source/src/app/blast/update_blastdb.pl#L184">184</a>
<a name=L185 href="source/src/app/blast/update_blastdb.pl#L185">185</a>
<a name=L186 href="source/src/app/blast/update_blastdb.pl#L186">186</a>
<a name=L187 href="source/src/app/blast/update_blastdb.pl#L187">187</a>
<a name=L188 href="source/src/app/blast/update_blastdb.pl#L188">188</a>
<a name=L189 href="source/src/app/blast/update_blastdb.pl#L189">189</a>
<a name=L190 href="source/src/app/blast/update_blastdb.pl#L190">190</a>
<a name=L191 href="source/src/app/blast/update_blastdb.pl#L191">191</a>
<a name=L192 href="source/src/app/blast/update_blastdb.pl#L192">192</a>
<a name=L193 href="source/src/app/blast/update_blastdb.pl#L193">193</a>
<a name=L194 href="source/src/app/blast/update_blastdb.pl#L194">194</a>
<a name=L195 href="source/src/app/blast/update_blastdb.pl#L195">195</a>
<a name=L196 href="source/src/app/blast/update_blastdb.pl#L196">196</a>
<a name=L197 href="source/src/app/blast/update_blastdb.pl#L197">197</a>
<a name=L198 href="source/src/app/blast/update_blastdb.pl#L198">198</a>
<a name=L199 href="source/src/app/blast/update_blastdb.pl#L199">199</a>
<a name=L200 href="source/src/app/blast/update_blastdb.pl#L200">200</a>
<a name=L201 href="source/src/app/blast/update_blastdb.pl#L201">201</a>
<a name=L202 href="source/src/app/blast/update_blastdb.pl#L202">202</a>
<a name=L203 href="source/src/app/blast/update_blastdb.pl#L203">203</a>
<a name=L204 href="source/src/app/blast/update_blastdb.pl#L204">204</a>
<a name=L205 href="source/src/app/blast/update_blastdb.pl#L205">205</a>
<a name=L206 href="source/src/app/blast/update_blastdb.pl#L206">206</a>
<a name=L207 href="source/src/app/blast/update_blastdb.pl#L207">207</a>
<a name=L208 href="source/src/app/blast/update_blastdb.pl#L208">208</a>
<a name=L209 href="source/src/app/blast/update_blastdb.pl#L209">209</a>
<a name=L210 href="source/src/app/blast/update_blastdb.pl#L210">210</a>
<a name=L211 href="source/src/app/blast/update_blastdb.pl#L211">211</a>
<a name=L212 href="source/src/app/blast/update_blastdb.pl#L212">212</a>
<a name=L213 href="source/src/app/blast/update_blastdb.pl#L213">213</a>
<a name=L214 href="source/src/app/blast/update_blastdb.pl#L214">214</a>
<a name=L215 href="source/src/app/blast/update_blastdb.pl#L215">215</a>
<a name=L216 href="source/src/app/blast/update_blastdb.pl#L216">216</a>
<a name=L217 href="source/src/app/blast/update_blastdb.pl#L217">217</a>
<a name=L218 href="source/src/app/blast/update_blastdb.pl#L218">218</a>
<a name=L219 href="source/src/app/blast/update_blastdb.pl#L219">219</a>
<a name=L220 href="source/src/app/blast/update_blastdb.pl#L220">220</a>
<a name=L221 href="source/src/app/blast/update_blastdb.pl#L221">221</a>
<a name=L222 href="source/src/app/blast/update_blastdb.pl#L222">222</a>
<a name=L223 href="source/src/app/blast/update_blastdb.pl#L223">223</a>
<a name=L224 href="source/src/app/blast/update_blastdb.pl#L224">224</a>
<a name=L225 href="source/src/app/blast/update_blastdb.pl#L225">225</a>
<a name=L226 href="source/src/app/blast/update_blastdb.pl#L226">226</a>
<a name=L227 href="source/src/app/blast/update_blastdb.pl#L227">227</a>
<a name=L228 href="source/src/app/blast/update_blastdb.pl#L228">228</a>
<a name=L229 href="source/src/app/blast/update_blastdb.pl#L229">229</a>
<a name=L230 href="source/src/app/blast/update_blastdb.pl#L230">230</a>
<a name=L231 href="source/src/app/blast/update_blastdb.pl#L231">231</a>
<a name=L232 href="source/src/app/blast/update_blastdb.pl#L232">232</a>
<a name=L233 href="source/src/app/blast/update_blastdb.pl#L233">233</a>
<a name=L234 href="source/src/app/blast/update_blastdb.pl#L234">234</a>
<a name=L235 href="source/src/app/blast/update_blastdb.pl#L235">235</a>
<a name=L236 href="source/src/app/blast/update_blastdb.pl#L236">236</a>
<a name=L237 href="source/src/app/blast/update_blastdb.pl#L237">237</a>
<a name=L238 href="source/src/app/blast/update_blastdb.pl#L238">238</a>
<a name=L239 href="source/src/app/blast/update_blastdb.pl#L239">239</a>
<a name=L240 href="source/src/app/blast/update_blastdb.pl#L240">240</a>
<a name=L241 href="source/src/app/blast/update_blastdb.pl#L241">241</a>
<a name=L242 href="source/src/app/blast/update_blastdb.pl#L242">242</a>
<a name=L243 href="source/src/app/blast/update_blastdb.pl#L243">243</a>
<a name=L244 href="source/src/app/blast/update_blastdb.pl#L244">244</a>
<a name=L245 href="source/src/app/blast/update_blastdb.pl#L245">245</a>
<a name=L246 href="source/src/app/blast/update_blastdb.pl#L246">246</a>
<a name=L247 href="source/src/app/blast/update_blastdb.pl#L247">247</a>
<a name=L248 href="source/src/app/blast/update_blastdb.pl#L248">248</a>
<a name=L249 href="source/src/app/blast/update_blastdb.pl#L249">249</a>
<a name=L250 href="source/src/app/blast/update_blastdb.pl#L250">250</a>
<a name=L251 href="source/src/app/blast/update_blastdb.pl#L251">251</a>
<a name=L252 href="source/src/app/blast/update_blastdb.pl#L252">252</a>
<a name=L253 href="source/src/app/blast/update_blastdb.pl#L253">253</a>
<a name=L254 href="source/src/app/blast/update_blastdb.pl#L254">254</a>
<a name=L255 href="source/src/app/blast/update_blastdb.pl#L255">255</a>
<a name=L256 href="source/src/app/blast/update_blastdb.pl#L256">256</a>
<a name=L257 href="source/src/app/blast/update_blastdb.pl#L257">257</a>
<a name=L258 href="source/src/app/blast/update_blastdb.pl#L258">258</a>
<a name=L259 href="source/src/app/blast/update_blastdb.pl#L259">259</a>
<a name=L260 href="source/src/app/blast/update_blastdb.pl#L260">260</a>
<a name=L261 href="source/src/app/blast/update_blastdb.pl#L261">261</a>
<a name=L262 href="source/src/app/blast/update_blastdb.pl#L262">262</a>
<a name=L263 href="source/src/app/blast/update_blastdb.pl#L263">263</a>
<a name=L264 href="source/src/app/blast/update_blastdb.pl#L264">264</a>
<a name=L265 href="source/src/app/blast/update_blastdb.pl#L265">265</a>
<a name=L266 href="source/src/app/blast/update_blastdb.pl#L266">266</a>
<a name=L267 href="source/src/app/blast/update_blastdb.pl#L267">267</a>
<a name=L268 href="source/src/app/blast/update_blastdb.pl#L268">268</a>
<a name=L269 href="source/src/app/blast/update_blastdb.pl#L269">269</a>
<a name=L270 href="source/src/app/blast/update_blastdb.pl#L270">270</a>
<a name=L271 href="source/src/app/blast/update_blastdb.pl#L271">271</a>
<a name=L272 href="source/src/app/blast/update_blastdb.pl#L272">272</a>
<a name=L273 href="source/src/app/blast/update_blastdb.pl#L273">273</a>
<a name=L274 href="source/src/app/blast/update_blastdb.pl#L274">274</a>
<a name=L275 href="source/src/app/blast/update_blastdb.pl#L275">275</a>
<a name=L276 href="source/src/app/blast/update_blastdb.pl#L276">276</a>
<a name=L277 href="source/src/app/blast/update_blastdb.pl#L277">277</a>
<a name=L278 href="source/src/app/blast/update_blastdb.pl#L278">278</a>
<a name=L279 href="source/src/app/blast/update_blastdb.pl#L279">279</a>
<a name=L280 href="source/src/app/blast/update_blastdb.pl#L280">280</a>
<a name=L281 href="source/src/app/blast/update_blastdb.pl#L281">281</a>
<a name=L282 href="source/src/app/blast/update_blastdb.pl#L282">282</a>
<a name=L283 href="source/src/app/blast/update_blastdb.pl#L283">283</a>
<a name=L284 href="source/src/app/blast/update_blastdb.pl#L284">284</a>
<a name=L285 href="source/src/app/blast/update_blastdb.pl#L285">285</a>
<a name=L286 href="source/src/app/blast/update_blastdb.pl#L286">286</a>
<a name=L287 href="source/src/app/blast/update_blastdb.pl#L287">287</a>
<a name=L288 href="source/src/app/blast/update_blastdb.pl#L288">288</a>
<a name=L289 href="source/src/app/blast/update_blastdb.pl#L289">289</a>
<a name=L290 href="source/src/app/blast/update_blastdb.pl#L290">290</a>
<a name=L291 href="source/src/app/blast/update_blastdb.pl#L291">291</a>
<a name=L292 href="source/src/app/blast/update_blastdb.pl#L292">292</a>
<a name=L293 href="source/src/app/blast/update_blastdb.pl#L293">293</a>
<a name=L294 href="source/src/app/blast/update_blastdb.pl#L294">294</a>
<a name=L295 href="source/src/app/blast/update_blastdb.pl#L295">295</a>
<a name=L296 href="source/src/app/blast/update_blastdb.pl#L296">296</a>
<a name=L297 href="source/src/app/blast/update_blastdb.pl#L297">297</a>
<a name=L298 href="source/src/app/blast/update_blastdb.pl#L298">298</a>
<a name=L299 href="source/src/app/blast/update_blastdb.pl#L299">299</a>
<a name=L300 href="source/src/app/blast/update_blastdb.pl#L300">300</a>
<a name=L301 href="source/src/app/blast/update_blastdb.pl#L301">301</a>
<a name=L302 href="source/src/app/blast/update_blastdb.pl#L302">302</a>
<a name=L303 href="source/src/app/blast/update_blastdb.pl#L303">303</a>
<a name=L304 href="source/src/app/blast/update_blastdb.pl#L304">304</a>
<a name=L305 href="source/src/app/blast/update_blastdb.pl#L305">305</a>
<a name=L306 href="source/src/app/blast/update_blastdb.pl#L306">306</a>
<a name=L307 href="source/src/app/blast/update_blastdb.pl#L307">307</a>
<a name=L308 href="source/src/app/blast/update_blastdb.pl#L308">308</a>
<a name=L309 href="source/src/app/blast/update_blastdb.pl#L309">309</a>
<a name=L310 href="source/src/app/blast/update_blastdb.pl#L310">310</a>
<a name=L311 href="source/src/app/blast/update_blastdb.pl#L311">311</a>
<a name=L312 href="source/src/app/blast/update_blastdb.pl#L312">312</a>
<a name=L313 href="source/src/app/blast/update_blastdb.pl#L313">313</a>
<a name=L314 href="source/src/app/blast/update_blastdb.pl#L314">314</a>
<a name=L315 href="source/src/app/blast/update_blastdb.pl#L315">315</a>
<a name=L316 href="source/src/app/blast/update_blastdb.pl#L316">316</a>
<a name=L317 href="source/src/app/blast/update_blastdb.pl#L317">317</a>
<a name=L318 href="source/src/app/blast/update_blastdb.pl#L318">318</a>
<a name=L319 href="source/src/app/blast/update_blastdb.pl#L319">319</a>
<a name=L320 href="source/src/app/blast/update_blastdb.pl#L320">320</a>
<a name=L321 href="source/src/app/blast/update_blastdb.pl#L321">321</a>
<a name=L322 href="source/src/app/blast/update_blastdb.pl#L322">322</a>
<a name=L323 href="source/src/app/blast/update_blastdb.pl#L323">323</a>
<a name=L324 href="source/src/app/blast/update_blastdb.pl#L324">324</a>
<a name=L325 href="source/src/app/blast/update_blastdb.pl#L325">325</a>
<a name=L326 href="source/src/app/blast/update_blastdb.pl#L326">326</a>
<a name=L327 href="source/src/app/blast/update_blastdb.pl#L327">327</a>
<a name=L328 href="source/src/app/blast/update_blastdb.pl#L328">328</a>
<a name=L329 href="source/src/app/blast/update_blastdb.pl#L329">329</a>
<a name=L330 href="source/src/app/blast/update_blastdb.pl#L330">330</a>
<a name=L331 href="source/src/app/blast/update_blastdb.pl#L331">331</a>
<a name=L332 href="source/src/app/blast/update_blastdb.pl#L332">332</a>
<a name=L333 href="source/src/app/blast/update_blastdb.pl#L333">333</a>
<a name=L334 href="source/src/app/blast/update_blastdb.pl#L334">334</a>
<a name=L335 href="source/src/app/blast/update_blastdb.pl#L335">335</a>
<a name=L336 href="source/src/app/blast/update_blastdb.pl#L336">336</a>
<a name=L337 href="source/src/app/blast/update_blastdb.pl#L337">337</a>
<a name=L338 href="source/src/app/blast/update_blastdb.pl#L338">338</a>
<a name=L339 href="source/src/app/blast/update_blastdb.pl#L339">339</a>
<a name=L340 href="source/src/app/blast/update_blastdb.pl#L340">340</a>
<a name=L341 href="source/src/app/blast/update_blastdb.pl#L341">341</a>
<a name=L342 href="source/src/app/blast/update_blastdb.pl#L342">342</a>
<a name=L343 href="source/src/app/blast/update_blastdb.pl#L343">343</a>
<a name=L344 href="source/src/app/blast/update_blastdb.pl#L344">344</a>
<a name=L345 href="source/src/app/blast/update_blastdb.pl#L345">345</a>
<a name=L346 href="source/src/app/blast/update_blastdb.pl#L346">346</a>
<a name=L347 href="source/src/app/blast/update_blastdb.pl#L347">347</a>
<a name=L348 href="source/src/app/blast/update_blastdb.pl#L348">348</a>
<a name=L349 href="source/src/app/blast/update_blastdb.pl#L349">349</a>
<a name=L350 href="source/src/app/blast/update_blastdb.pl#L350">350</a>
<a name=L351 href="source/src/app/blast/update_blastdb.pl#L351">351</a>
<a name=L352 href="source/src/app/blast/update_blastdb.pl#L352">352</a>
<a name=L353 href="source/src/app/blast/update_blastdb.pl#L353">353</a>
<a name=L354 href="source/src/app/blast/update_blastdb.pl#L354">354</a>
<a name=L355 href="source/src/app/blast/update_blastdb.pl#L355">355</a>
<a name=L356 href="source/src/app/blast/update_blastdb.pl#L356">356</a>
<a name=L357 href="source/src/app/blast/update_blastdb.pl#L357">357</a>
<a name=L358 href="source/src/app/blast/update_blastdb.pl#L358">358</a>
<a name=L359 href="source/src/app/blast/update_blastdb.pl#L359">359</a>
<a name=L360 href="source/src/app/blast/update_blastdb.pl#L360">360</a>
<a name=L361 href="source/src/app/blast/update_blastdb.pl#L361">361</a>
<a name=L362 href="source/src/app/blast/update_blastdb.pl#L362">362</a>
<a name=L363 href="source/src/app/blast/update_blastdb.pl#L363">363</a>
<a name=L364 href="source/src/app/blast/update_blastdb.pl#L364">364</a>
<a name=L365 href="source/src/app/blast/update_blastdb.pl#L365">365</a>
<a name=L366 href="source/src/app/blast/update_blastdb.pl#L366">366</a>
<a name=L367 href="source/src/app/blast/update_blastdb.pl#L367">367</a>
<a name=L368 href="source/src/app/blast/update_blastdb.pl#L368">368</a>
<a name=L369 href="source/src/app/blast/update_blastdb.pl#L369">369</a>
<a name=L370 href="source/src/app/blast/update_blastdb.pl#L370">370</a>
<a name=L371 href="source/src/app/blast/update_blastdb.pl#L371">371</a>
<a name=L372 href="source/src/app/blast/update_blastdb.pl#L372">372</a>
<a name=L373 href="source/src/app/blast/update_blastdb.pl#L373">373</a>
<a name=L374 href="source/src/app/blast/update_blastdb.pl#L374">374</a>
<a name=L375 href="source/src/app/blast/update_blastdb.pl#L375">375</a>
<a name=L376 href="source/src/app/blast/update_blastdb.pl#L376">376</a>
<a name=L377 href="source/src/app/blast/update_blastdb.pl#L377">377</a>
<a name=L378 href="source/src/app/blast/update_blastdb.pl#L378">378</a>
<a name=L379 href="source/src/app/blast/update_blastdb.pl#L379">379</a>
<a name=L380 href="source/src/app/blast/update_blastdb.pl#L380">380</a>
<a name=L381 href="source/src/app/blast/update_blastdb.pl#L381">381</a>
<a name=L382 href="source/src/app/blast/update_blastdb.pl#L382">382</a>
<a name=L383 href="source/src/app/blast/update_blastdb.pl#L383">383</a>
<a name=L384 href="source/src/app/blast/update_blastdb.pl#L384">384</a>
<a name=L385 href="source/src/app/blast/update_blastdb.pl#L385">385</a>
<a name=L386 href="source/src/app/blast/update_blastdb.pl#L386">386</a>
<a name=L387 href="source/src/app/blast/update_blastdb.pl#L387">387</a>
<a name=L388 href="source/src/app/blast/update_blastdb.pl#L388">388</a>
<a name=L389 href="source/src/app/blast/update_blastdb.pl#L389">389</a>
<a name=L390 href="source/src/app/blast/update_blastdb.pl#L390">390</a>
<a name=L391 href="source/src/app/blast/update_blastdb.pl#L391">391</a>
<a name=L392 href="source/src/app/blast/update_blastdb.pl#L392">392</a>
<a name=L393 href="source/src/app/blast/update_blastdb.pl#L393">393</a>
<a name=L394 href="source/src/app/blast/update_blastdb.pl#L394">394</a>
<a name=L395 href="source/src/app/blast/update_blastdb.pl#L395">395</a>
<a name=L396 href="source/src/app/blast/update_blastdb.pl#L396">396</a>
<a name=L397 href="source/src/app/blast/update_blastdb.pl#L397">397</a>
<a name=L398 href="source/src/app/blast/update_blastdb.pl#L398">398</a>
<a name=L399 href="source/src/app/blast/update_blastdb.pl#L399">399</a>
<a name=L400 href="source/src/app/blast/update_blastdb.pl#L400">400</a>
<a name=L401 href="source/src/app/blast/update_blastdb.pl#L401">401</a>
<a name=L402 href="source/src/app/blast/update_blastdb.pl#L402">402</a>
<a name=L403 href="source/src/app/blast/update_blastdb.pl#L403">403</a>
<a name=L404 href="source/src/app/blast/update_blastdb.pl#L404">404</a>
<a name=L405 href="source/src/app/blast/update_blastdb.pl#L405">405</a>
<a name=L406 href="source/src/app/blast/update_blastdb.pl#L406">406</a>
<a name=L407 href="source/src/app/blast/update_blastdb.pl#L407">407</a>
<a name=L408 href="source/src/app/blast/update_blastdb.pl#L408">408</a>
<a name=L409 href="source/src/app/blast/update_blastdb.pl#L409">409</a>
<a name=L410 href="source/src/app/blast/update_blastdb.pl#L410">410</a>
<a name=L411 href="source/src/app/blast/update_blastdb.pl#L411">411</a>
<a name=L412 href="source/src/app/blast/update_blastdb.pl#L412">412</a>
<a name=L413 href="source/src/app/blast/update_blastdb.pl#L413">413</a>
<a name=L414 href="source/src/app/blast/update_blastdb.pl#L414">414</a>
<a name=L415 href="source/src/app/blast/update_blastdb.pl#L415">415</a>
<a name=L416 href="source/src/app/blast/update_blastdb.pl#L416">416</a>
<a name=L417 href="source/src/app/blast/update_blastdb.pl#L417">417</a>
<a name=L418 href="source/src/app/blast/update_blastdb.pl#L418">418</a>
<a name=L419 href="source/src/app/blast/update_blastdb.pl#L419">419</a>
<a name=L420 href="source/src/app/blast/update_blastdb.pl#L420">420</a>
<a name=L421 href="source/src/app/blast/update_blastdb.pl#L421">421</a>
<a name=L422 href="source/src/app/blast/update_blastdb.pl#L422">422</a>
<a name=L423 href="source/src/app/blast/update_blastdb.pl#L423">423</a>
<a name=L424 href="source/src/app/blast/update_blastdb.pl#L424">424</a>
<a name=L425 href="source/src/app/blast/update_blastdb.pl#L425">425</a>
<a name=L426 href="source/src/app/blast/update_blastdb.pl#L426">426</a>
<a name=L427 href="source/src/app/blast/update_blastdb.pl#L427">427</a>
<a name=L428 href="source/src/app/blast/update_blastdb.pl#L428">428</a>
<a name=L429 href="source/src/app/blast/update_blastdb.pl#L429">429</a>
<a name=L430 href="source/src/app/blast/update_blastdb.pl#L430">430</a>
<a name=L431 href="source/src/app/blast/update_blastdb.pl#L431">431</a>
<a name=L432 href="source/src/app/blast/update_blastdb.pl#L432">432</a>
<a name=L433 href="source/src/app/blast/update_blastdb.pl#L433">433</a>
<a name=L434 href="source/src/app/blast/update_blastdb.pl#L434">434</a>
<a name=L435 href="source/src/app/blast/update_blastdb.pl#L435">435</a>
<a name=L436 href="source/src/app/blast/update_blastdb.pl#L436">436</a>
<a name=L437 href="source/src/app/blast/update_blastdb.pl#L437">437</a>
<a name=L438 href="source/src/app/blast/update_blastdb.pl#L438">438</a>
<a name=L439 href="source/src/app/blast/update_blastdb.pl#L439">439</a>
<a name=L440 href="source/src/app/blast/update_blastdb.pl#L440">440</a>
<a name=L441 href="source/src/app/blast/update_blastdb.pl#L441">441</a>
<a name=L442 href="source/src/app/blast/update_blastdb.pl#L442">442</a>
<a name=L443 href="source/src/app/blast/update_blastdb.pl#L443">443</a>
<a name=L444 href="source/src/app/blast/update_blastdb.pl#L444">444</a>
<a name=L445 href="source/src/app/blast/update_blastdb.pl#L445">445</a>
<a name=L446 href="source/src/app/blast/update_blastdb.pl#L446">446</a>
<a name=L447 href="source/src/app/blast/update_blastdb.pl#L447">447</a>
<a name=L448 href="source/src/app/blast/update_blastdb.pl#L448">448</a>
<a name=L449 href="source/src/app/blast/update_blastdb.pl#L449">449</a>
<a name=L450 href="source/src/app/blast/update_blastdb.pl#L450">450</a>
<a name=L451 href="source/src/app/blast/update_blastdb.pl#L451">451</a>
<a name=L452 href="source/src/app/blast/update_blastdb.pl#L452">452</a>
<a name=L453 href="source/src/app/blast/update_blastdb.pl#L453">453</a>
<a name=L454 href="source/src/app/blast/update_blastdb.pl#L454">454</a>
<a name=L455 href="source/src/app/blast/update_blastdb.pl#L455">455</a>
<a name=L456 href="source/src/app/blast/update_blastdb.pl#L456">456</a>
<a name=L457 href="source/src/app/blast/update_blastdb.pl#L457">457</a>
<a name=L458 href="source/src/app/blast/update_blastdb.pl#L458">458</a>
<a name=L459 href="source/src/app/blast/update_blastdb.pl#L459">459</a>
<a name=L460 href="source/src/app/blast/update_blastdb.pl#L460">460</a>
<a name=L461 href="source/src/app/blast/update_blastdb.pl#L461">461</a>
<a name=L462 href="source/src/app/blast/update_blastdb.pl#L462">462</a>
<a name=L463 href="source/src/app/blast/update_blastdb.pl#L463">463</a>
<a name=L464 href="source/src/app/blast/update_blastdb.pl#L464">464</a>
<a name=L465 href="source/src/app/blast/update_blastdb.pl#L465">465</a>
<a name=L466 href="source/src/app/blast/update_blastdb.pl#L466">466</a>
<a name=L467 href="source/src/app/blast/update_blastdb.pl#L467">467</a>
<a name=L468 href="source/src/app/blast/update_blastdb.pl#L468">468</a>
<a name=L469 href="source/src/app/blast/update_blastdb.pl#L469">469</a>
<a name=L470 href="source/src/app/blast/update_blastdb.pl#L470">470</a>
<a name=L471 href="source/src/app/blast/update_blastdb.pl#L471">471</a>
<a name=L472 href="source/src/app/blast/update_blastdb.pl#L472">472</a>
<a name=L473 href="source/src/app/blast/update_blastdb.pl#L473">473</a>
<a name=L474 href="source/src/app/blast/update_blastdb.pl#L474">474</a>
<a name=L475 href="source/src/app/blast/update_blastdb.pl#L475">475</a>
<a name=L476 href="source/src/app/blast/update_blastdb.pl#L476">476</a>
<a name=L477 href="source/src/app/blast/update_blastdb.pl#L477">477</a>
<a name=L478 href="source/src/app/blast/update_blastdb.pl#L478">478</a>
<a name=L479 href="source/src/app/blast/update_blastdb.pl#L479">479</a>
<a name=L480 href="source/src/app/blast/update_blastdb.pl#L480">480</a>
<a name=L481 href="source/src/app/blast/update_blastdb.pl#L481">481</a>
<a name=L482 href="source/src/app/blast/update_blastdb.pl#L482">482</a>
<a name=L483 href="source/src/app/blast/update_blastdb.pl#L483">483</a>
<a name=L484 href="source/src/app/blast/update_blastdb.pl#L484">484</a>
<a name=L485 href="source/src/app/blast/update_blastdb.pl#L485">485</a>
<a name=L486 href="source/src/app/blast/update_blastdb.pl#L486">486</a>
<a name=L487 href="source/src/app/blast/update_blastdb.pl#L487">487</a>
<a name=L488 href="source/src/app/blast/update_blastdb.pl#L488">488</a>
<a name=L489 href="source/src/app/blast/update_blastdb.pl#L489">489</a>
<a name=L490 href="source/src/app/blast/update_blastdb.pl#L490">490</a>
<a name=L491 href="source/src/app/blast/update_blastdb.pl#L491">491</a>
<a name=L492 href="source/src/app/blast/update_blastdb.pl#L492">492</a>
<a name=L493 href="source/src/app/blast/update_blastdb.pl#L493">493</a>
<a name=L494 href="source/src/app/blast/update_blastdb.pl#L494">494</a>
<a name=L495 href="source/src/app/blast/update_blastdb.pl#L495">495</a>
<a name=L496 href="source/src/app/blast/update_blastdb.pl#L496">496</a>
<a name=L497 href="source/src/app/blast/update_blastdb.pl#L497">497</a>
<a name=L498 href="source/src/app/blast/update_blastdb.pl#L498">498</a>
<a name=L499 href="source/src/app/blast/update_blastdb.pl#L499">499</a>
<a name=L500 href="source/src/app/blast/update_blastdb.pl#L500">500</a>
<a name=L501 href="source/src/app/blast/update_blastdb.pl#L501">501</a>
<a name=L502 href="source/src/app/blast/update_blastdb.pl#L502">502</a>
<a name=L503 href="source/src/app/blast/update_blastdb.pl#L503">503</a>
<a name=L504 href="source/src/app/blast/update_blastdb.pl#L504">504</a>
<a name=L505 href="source/src/app/blast/update_blastdb.pl#L505">505</a>
<a name=L506 href="source/src/app/blast/update_blastdb.pl#L506">506</a>
<a name=L507 href="source/src/app/blast/update_blastdb.pl#L507">507</a>
<a name=L508 href="source/src/app/blast/update_blastdb.pl#L508">508</a>
<a name=L509 href="source/src/app/blast/update_blastdb.pl#L509">509</a>
<a name=L510 href="source/src/app/blast/update_blastdb.pl#L510">510</a>
<a name=L511 href="source/src/app/blast/update_blastdb.pl#L511">511</a>
<a name=L512 href="source/src/app/blast/update_blastdb.pl#L512">512</a>
<a name=L513 href="source/src/app/blast/update_blastdb.pl#L513">513</a>
<a name=L514 href="source/src/app/blast/update_blastdb.pl#L514">514</a>
<a name=L515 href="source/src/app/blast/update_blastdb.pl#L515">515</a>
<a name=L516 href="source/src/app/blast/update_blastdb.pl#L516">516</a>
<a name=L517 href="source/src/app/blast/update_blastdb.pl#L517">517</a>
<a name=L518 href="source/src/app/blast/update_blastdb.pl#L518">518</a>
<a name=L519 href="source/src/app/blast/update_blastdb.pl#L519">519</a>
<a name=L520 href="source/src/app/blast/update_blastdb.pl#L520">520</a>
<a name=L521 href="source/src/app/blast/update_blastdb.pl#L521">521</a>
<a name=L522 href="source/src/app/blast/update_blastdb.pl#L522">522</a>
<a name=L523 href="source/src/app/blast/update_blastdb.pl#L523">523</a>
<a name=L524 href="source/src/app/blast/update_blastdb.pl#L524">524</a>
<a name=L525 href="source/src/app/blast/update_blastdb.pl#L525">525</a>
<a name=L526 href="source/src/app/blast/update_blastdb.pl#L526">526</a>
<a name=L527 href="source/src/app/blast/update_blastdb.pl#L527">527</a>
<a name=L528 href="source/src/app/blast/update_blastdb.pl#L528">528</a>
<a name=L529 href="source/src/app/blast/update_blastdb.pl#L529">529</a>
<a name=L530 href="source/src/app/blast/update_blastdb.pl#L530">530</a>
<a name=L531 href="source/src/app/blast/update_blastdb.pl#L531">531</a>
<a name=L532 href="source/src/app/blast/update_blastdb.pl#L532">532</a>
<a name=L533 href="source/src/app/blast/update_blastdb.pl#L533">533</a>
<a name=L534 href="source/src/app/blast/update_blastdb.pl#L534">534</a>
<a name=L535 href="source/src/app/blast/update_blastdb.pl#L535">535</a>
<a name=L536 href="source/src/app/blast/update_blastdb.pl#L536">536</a>
<a name=L537 href="source/src/app/blast/update_blastdb.pl#L537">537</a>
<a name=L538 href="source/src/app/blast/update_blastdb.pl#L538">538</a>
<a name=L539 href="source/src/app/blast/update_blastdb.pl#L539">539</a>
<a name=L540 href="source/src/app/blast/update_blastdb.pl#L540">540</a>
<a name=L541 href="source/src/app/blast/update_blastdb.pl#L541">541</a>
<a name=L542 href="source/src/app/blast/update_blastdb.pl#L542">542</a>
<a name=L543 href="source/src/app/blast/update_blastdb.pl#L543">543</a>
<a name=L544 href="source/src/app/blast/update_blastdb.pl#L544">544</a>
<a name=L545 href="source/src/app/blast/update_blastdb.pl#L545">545</a>
<a name=L546 href="source/src/app/blast/update_blastdb.pl#L546">546</a>
<a name=L547 href="source/src/app/blast/update_blastdb.pl#L547">547</a>
<a name=L548 href="source/src/app/blast/update_blastdb.pl#L548">548</a>
<a name=L549 href="source/src/app/blast/update_blastdb.pl#L549">549</a>
<a name=L550 href="source/src/app/blast/update_blastdb.pl#L550">550</a>
<a name=L551 href="source/src/app/blast/update_blastdb.pl#L551">551</a>
<a name=L552 href="source/src/app/blast/update_blastdb.pl#L552">552</a>
<a name=L553 href="source/src/app/blast/update_blastdb.pl#L553">553</a>
<a name=L554 href="source/src/app/blast/update_blastdb.pl#L554">554</a>
<a name=L555 href="source/src/app/blast/update_blastdb.pl#L555">555</a>
<a name=L556 href="source/src/app/blast/update_blastdb.pl#L556">556</a>
<a name=L557 href="source/src/app/blast/update_blastdb.pl#L557">557</a>
<a name=L558 href="source/src/app/blast/update_blastdb.pl#L558">558</a>
<a name=L559 href="source/src/app/blast/update_blastdb.pl#L559">559</a>
<a name=L560 href="source/src/app/blast/update_blastdb.pl#L560">560</a>
<a name=L561 href="source/src/app/blast/update_blastdb.pl#L561">561</a>
<a name=L562 href="source/src/app/blast/update_blastdb.pl#L562">562</a>
<a name=L563 href="source/src/app/blast/update_blastdb.pl#L563">563</a>
<a name=L564 href="source/src/app/blast/update_blastdb.pl#L564">564</a>
<a name=L565 href="source/src/app/blast/update_blastdb.pl#L565">565</a>
<a name=L566 href="source/src/app/blast/update_blastdb.pl#L566">566</a>
<a name=L567 href="source/src/app/blast/update_blastdb.pl#L567">567</a>
<a name=L568 href="source/src/app/blast/update_blastdb.pl#L568">568</a>
<a name=L569 href="source/src/app/blast/update_blastdb.pl#L569">569</a>
<a name=L570 href="source/src/app/blast/update_blastdb.pl#L570">570</a>
<a name=L571 href="source/src/app/blast/update_blastdb.pl#L571">571</a>
<a name=L572 href="source/src/app/blast/update_blastdb.pl#L572">572</a>
<a name=L573 href="source/src/app/blast/update_blastdb.pl#L573">573</a>
<a name=L574 href="source/src/app/blast/update_blastdb.pl#L574">574</a>
<a name=L575 href="source/src/app/blast/update_blastdb.pl#L575">575</a>
</div><div class="src-content">#!/usr/bin/perl
# $Id: update_blastdb.pl 84763 2018-12-07 21:43:26Z camacho $
# ===========================================================================
#
#                            PUBLIC DOMAIN NOTICE
#               National Center for Biotechnology Information
#
#  This software/database is a "United States Government Work" under the
#  terms of the United States Copyright Act.  It was written as part of
#  the author's official duties as a United States Government employee and
#  thus cannot be copyrighted.  This software/database is freely available
#  to the public for use. The National Library of Medicine and the U.S.
#  Government have not placed any restriction on its use or reproduction.
#
#  Although all reasonable efforts have been taken to ensure the accuracy
#  and reliability of the software and data, the NLM and the U.S.
#  Government do not and cannot warrant the performance or results that
#  may be obtained by using this software or data. The NLM and the U.S.
#  Government disclaim all warranties, express or implied, including
#  warranties of performance, merchantability or fitness for any particular
#  purpose.
#
#  Please cite the author in any work or product based on this material.
#
# ===========================================================================
#
# Author:  Christiam Camacho
#
# File Description:
#   Script to download the pre-formatted BLAST databases.
#
# ===========================================================================

use strict;
use warnings;
use Net::FTP;
use Getopt::Long;
use Pod::Usage;
use File::stat;
use Digest::MD5;
use Archive::Tar;
use File::Temp;
use JSON;

use constant NCBI_FTP =&gt; "ftp.ncbi.nlm.nih.gov";
use constant BLAST_DB_DIR =&gt; "/blast/db";
use constant USER =&gt; "anonymous";
use constant PASSWORD =&gt; "anonymous";
use constant DEBUG =&gt; 0;
use constant MAX_DOWNLOAD_ATTEMPTS =&gt; 3;
use constant EXIT_FAILURE =&gt; 2;

use constant GCS_URL =&gt; "<a href="https://storage.googleapis.com";">https://storage.googleapis.com";</a>
use constant GCP_URL =&gt; "<a href="http://metadata.google.internal/computeMetadata/v1/instance/id";">http://metadata.google.internal/computeMetadata/v1/instance/id";</a>
use constant GCP_BUCKET =&gt; "blast-db";
use constant GCP_MANIFEST =&gt; "blastdb-manifest.json";
use constant GCP_MANIFEST_VERSION =&gt; "1.0";

# Process command line options
my $opt_verbose = 1;
my $opt_quiet = 0;
my $opt_force_download = 0;     
my $opt_help = 0;
my $opt_passive = 1;
my $opt_blastdb_ver = 4;
my $opt_timeout = 120;
my $opt_showall = undef;
my $opt_show_version = 0;
my $opt_decompress = 0;
my $opt_source;
my $opt_nt = &amp;get_num_cores();
my $result = GetOptions("verbose+"      =&gt;  \$opt_verbose,
                        "quiet"         =&gt;  \$opt_quiet,
                        "force"         =&gt;  \$opt_force_download,
                        "passive:s"     =&gt;  \$opt_passive,
                        "timeout=i"     =&gt;  \$opt_timeout,
                        "showall:s"     =&gt;  \$opt_showall,
                        "version"       =&gt;  \$opt_show_version,
                        "blastdb_version:i"=&gt;  \$opt_blastdb_ver,
                        "decompress"    =&gt;  \$opt_decompress,
                        "source=s"      =&gt;  \$opt_source,
                        "num_threads=i" =&gt;  \$opt_nt,
                        "help"          =&gt;  \$opt_help);
$opt_verbose = 0 if $opt_quiet;
die "Failed to parse command line options\n" unless $result;
pod2usage({-exitval =&gt; 0, -verbose =&gt; 2}) if $opt_help;
if (length($opt_passive) and ($opt_passive !~ /1|no/i)) {
    pod2usage({-exitval =&gt; 1, -verbose =&gt; 0,
            -msg =&gt; "Invalid value for passive option: '$opt_passive'"});
}
pod2usage({-exitval =&gt; 0, -verbose =&gt; 2}) unless (scalar @ARGV or 
                                                  defined($opt_showall) or
                                                  $opt_show_version);
pod2usage({-exitval =&gt; 1, -verbose =&gt; 0, -msg =&gt; "Invalid BLAST database version"}) 
    unless ($opt_blastdb_ver == 4 or $opt_blastdb_ver == 5);
pod2usage({-exitval =&gt; 1, -verbose =&gt; 0, -msg =&gt; "Invalid number of threads"}) 
    if ($opt_nt &lt;= 0);
if (length($opt_passive) and $opt_passive =~ /n|no/i) {
    $opt_passive = 0;
} else {
    $opt_passive = 1;
}
my $exit_code = 0;
$|++;

my $location = "NCBI";
unless ($^O =~ /mswin/i) {
    $location = system("/usr/bin/curl -sfo /dev/null -H 'Metadata-Flavor: Google' " . GCP_URL) == 0 ? "GCP" : "NCBI";
}
# Override data source, only for testing
if (defined($opt_source)) {
    if ($opt_source =~ /^ncbi/i) {
        $location = "NCBI";
    } elsif ($opt_source =~ /^gc/i and $^O !~ /mswin/i) {
        $location = "GCP";
    }
}

if ($opt_show_version) {
    my $revision = '$Revision: 84763 $';
    $revision =~ s/\$Revision: | \$//g;
    print "$0 version $revision\n";
    exit($exit_code);
}

my $ftp;

if ($location eq "GCP") {
    #die "Only BLASTDB vesion 5 is supported at GCP\n" if ($opt_blastdb_ver == 4);
    my $latest_dir = &amp;get_gcs_latest_dir();
    my ($json, $url) = &amp;get_gcs_blastdb_metadata($latest_dir);
    unless (length($json)) {
        print STDERR "ERROR: Missing manifest file $url, please report to blast-help\@ncbi.nlm.nih.gov\n";
        exit(2);
    }
    print "Connected to $location\n" if $opt_verbose;
    my $metadata = from_json($json);
    unless (exists($$metadata{version}) and ($$metadata{version} eq GCP_MANIFEST_VERSION)) {
        print STDERR "ERROR: Invalid version in manifest file $url, please report to blast-help\@ncbi.nlm.nih.gov\n";
        exit(2);
    }
    if (defined($opt_showall)) {
        my $print_header = 1;
        foreach my $db (sort keys %$metadata) {
            next if ($db =~ /^version$/);
            if ($opt_showall =~ /tsv/i) {
                printf("%s\t%s\t%9.4f\t%s\n", $db, $$metadata{$db}{description}, 
                    $$metadata{$db}{size}, $$metadata{$db}{last_updated});
            } elsif ($opt_showall =~ /pretty/i) {
                if ($print_header) {
                    printf("%-60s %-120s %-11s %15s\n", "BLASTDB", 
                        "DESCRIPTION", "SIZE (GB)", "LAST_UPDATED");
                    $print_header = 0;
                }
                printf("%-60s %-120s %9.4f %15s\n", $db, $$metadata{$db}{description}, 
                    $$metadata{$db}{size}, $$metadata{$db}{last_updated});
            } else {
                print "$db\n";
            }
        }
    } else {
        my @files2download;
        for my $requested_db (@ARGV) {
            if (exists $$metadata{$requested_db}) {
                push @files2download, @{$$metadata{$requested_db}{files}};
            } else {
                print STDERR "Warning: $requested_db does not exist in $location ($latest_dir)\n";
            }
        }
        if (@files2download) {
            my $gsutil = &amp;get_gsutil_path();
            my $cmd;
            my $fh = File::Temp-&gt;new();
            if (defined($gsutil)) {
                $cmd = "$gsutil " . ($opt_nt &gt; 1 ? "-m" : "" ) . " -q cp ";
                $cmd .= join(" ", @files2download) . " .";
            } else { # fall back to  curl
                my $url = GCS_URL;
                s,gs://,$url/, foreach (@files2download);
                if ($opt_nt &gt; 1 and -f "/usr/bin/xargs") {
                    print $fh join("\n", @files2download);
                    $cmd = "/usr/bin/xargs -P $opt_nt -a $fh -n 1";
                    $cmd .= " -t" if $opt_verbose &gt; 3;
                    $cmd .= " /usr/bin/curl -sO";
                } else {
                    $cmd = "/usr/bin/curl -s";
                    $cmd .= " -O $_" foreach (@files2download);
                }
            }
            print "$cmd\n" if $opt_verbose &gt; 3;
            system($cmd);
        }
    }

} else {
    # Connect and download files
    $ftp = &amp;connect_to_ftp();
    if (defined $opt_showall) {
        print "$_\n" foreach (sort(&amp;get_available_databases($ftp-&gt;ls())));
    } else {
        my @files = sort(&amp;get_files_to_download());
        my @files2decompress;
        $exit_code = &amp;download(\@files, \@files2decompress);
        if ($exit_code == 1) {
            foreach (@files2decompress) {
                $exit_code = &amp;decompress($_);
                last if ($exit_code != 1);
            }
        }
    }
    $ftp-&gt;quit();
}

exit($exit_code);

# Connects to NCBI ftp server
sub connect_to_ftp
{
    my %ftp_opts;
    $ftp_opts{'Passive'} = 1 if $opt_passive;
    $ftp_opts{'Timeout'} = $opt_timeout if ($opt_timeout &gt;= 0);
    $ftp_opts{'Debug'}   = 1 if ($opt_verbose &gt; 1);
    my $ftp = Net::FTP-&gt;new(NCBI_FTP, %ftp_opts)
        or die "Failed to connect to " . NCBI_FTP . ": $!\n";
    $ftp-&gt;login(USER, PASSWORD) 
        or die "Failed to login to " . NCBI_FTP . ": $!\n";
    my $ftp_path = BLAST_DB_DIR;
    $ftp_path .= "/v5" if ($opt_blastdb_ver == 5);
    $ftp-&gt;cwd($ftp_path);
    $ftp-&gt;binary();
    print "Connected to $location\n" if $opt_verbose;
    return $ftp;
}

# Gets the list of available databases on NCBI FTP site
sub get_available_databases
{
    my @blast_db_files = $ftp-&gt;ls();
    my @retval = ();

    foreach (@blast_db_files) {
        next unless (/\.tar\.gz$/);
        push @retval, &amp;extract_db_name($_);
    }
    my %seen = ();
    return grep { ! $seen{$_} ++ } @retval;
}

# Obtains the list of files to download
sub get_files_to_download
{
    my @blast_db_files = $ftp-&gt;ls();
    my @retval = ();

    if ($opt_verbose &gt; 2) {
        print "Found the following files on ftp site:\n";
        print "$_\n" for (@blast_db_files);
    }

    if (grep(/gss/, @ARGV) and not grep(/gss_annot/, @ARGV)) {
        push @ARGV, qw(gss_annot);
    }

    for my $requested_db (@ARGV) {
        for my $file (@blast_db_files) {
            next unless ($file =~ /\.tar\.gz$/);    
            if ($file =~ /^$requested_db\..*/) {
                push @retval, $file;
            }
        }
    }

    if ($opt_verbose) {
        for my $requested_db (@ARGV) {
            unless (grep(/$requested_db/, @retval)) {
                print STDERR "$requested_db not found, skipping.\n" 
            }
        }
    }

    return @retval;
}

# Download the requested files only if their checksum files are missing or if
# these (or the archives) are newer in the FTP site. Returns 0 if no files were
# downloaded, 1 if at least one file was downloaded (so that this can be the
# application's exit code)
sub download($$)
{
    my @requested_dbs = @ARGV;
    my @files2download = @{$_[0]};
    my $files2decompress = $_[1];
    my $retval = 0;

    for my $file (@files2download) {

        my $attempts = 0;   # Download attempts for this file
        if ($opt_verbose and &amp;is_multivolume_db($file) and $file =~ /\.00\./) {
            my $db_name = &amp;extract_db_name($file);
            my $nvol = &amp;get_num_volumes($db_name, @files2download);
            print "Downloading $db_name (" . $nvol . " volumes) ...\n" unless ($opt_quiet);
        }

        # We preserve the checksum files as evidence of the downloaded archive
        my $checksum_file = "$file.md5";
        my $new_download = (-e $checksum_file ? 0 : 1);
        my $update_available = ($new_download or 
                    ((stat($checksum_file))-&gt;mtime &lt; $ftp-&gt;mdtm($checksum_file)));
        if (-e $file and (stat($file)-&gt;mtime &lt; $ftp-&gt;mdtm($file))) {
            $update_available = 1;
        }

download_file:
        if ($opt_force_download or $new_download or $update_available) {
            print "Downloading $file..." if $opt_verbose;
            $ftp-&gt;get($file);
            unless ($ftp-&gt;get($checksum_file)) {
                print STDERR "Failed to download $checksum_file!\n";
                return EXIT_FAILURE;
            }
            my $rmt_digest = &amp;read_md5_file($checksum_file);
            my $lcl_digest = &amp;compute_md5_checksum($file);
            print "\nRMT $file Digest $rmt_digest" if (DEBUG);
            print "\nLCL $file Digest $lcl_digest\n" if (DEBUG);
            if ($lcl_digest ne $rmt_digest) {
                unlink $file, $checksum_file;
                if (++$attempts &gt;= MAX_DOWNLOAD_ATTEMPTS) {
                    print STDERR "too many failures, aborting download!\n";
                    return EXIT_FAILURE;
                } else {
                    print "corrupt download, trying again.\n";
                    goto download_file;
                }
            }
            push @$files2decompress, $file if ($opt_decompress);
            print " [OK]\n" if $opt_verbose;
            $retval = 1 if ($retval == 0);
        } else {
            if ($opt_decompress and -f $file) {
                push @$files2decompress, $file;
                $retval = 1;
            } else {
                my $msg = ($opt_decompress 
                           ? "The contents of $file are up to date in your system." 
                           : "$file is up to date.");
                print "$msg\n" if $opt_verbose;
            }
        }
    }
    return $retval;
}

# Try to decompress using /bin/tar as Archive::Tar is known to be slower (as
# it's pure perl)
sub _decompress_impl($)
{
    my $file = shift;
    unless ($^O =~ /win/i) {
        local $ENV{PATH} = "/bin:/usr/bin";
        my $cmd = "gzip -cd $file 2&gt;/dev/null | tar xf - 2&gt;/dev/null";
        return 1 unless (system($cmd));
    }
    return Archive::Tar-&gt;extract_archive($file, 1);
}

# Decompresses the file passed as its argument
# Returns 1 on success, and 2 on failure, printing an error to STDERR
sub decompress($)
{
    my $file = shift;
    print "Decompressing $file ..." unless ($opt_quiet);
    my $succeeded = &amp;_decompress_impl($file);
    unless ($succeeded) {
        my $msg = "Failed to decompress $file ($Archive::Tar::error), ";
        $msg .= "please do so manually.";
        print STDERR "$msg\n";
        return EXIT_FAILURE;
    }
    unlink $file;   # Clean up archive, but preserve the checksum file
    print " [OK]\n" unless ($opt_quiet);
    return 1;
}

sub compute_md5_checksum($)
{
    my $file = shift;
    my $digest = "N/A";
    if (open(DOWNLOADED_FILE, $file)) {
        binmode(DOWNLOADED_FILE);
        $digest = Digest::MD5-&gt;new-&gt;addfile(*DOWNLOADED_FILE)-&gt;hexdigest;
        close(DOWNLOADED_FILE);
    }
    return $digest;
}

sub read_md5_file($)
{
    my $md5file = shift;
    open(IN, $md5file);
    $_ = &lt;IN&gt;;
    close(IN);
    my @retval = split;
    return $retval[0];
}

# Determine if a given pre-formatted BLAST database file is part of a
# multi-volume database
sub is_multivolume_db
{
    my $file = shift;
    return 1 if ($file =~ /\.\d{2,3}\.tar\.gz$/);
    return 0;
}

# Extracts the database name from the pre-formatted BLAST database archive file
# name
sub extract_db_name
{
    my $file = shift;
    my $retval = "";
    if (&amp;is_multivolume_db($file)) {
        $retval = $1 if ($file =~ m/(.*)\.\d{2,3}\.tar\.gz$/);
    } else {
        $retval = $1 if ($file =~ m/(.*)\.tar\.gz$/);
    }
    return $retval;
}

# Returns the number of volumes for a BLAST database given the file name of a
# pre-formatted BLAST database and the list of all databases to download
sub get_num_volumes
{
    my $db = shift;
    my $retval = 0;
    foreach (@_) {
        if (/$db/) {
            if (/.*\.(\d{2,3})\.tar\.gz$/) {
                $retval = int($1) if (int($1) &gt; $retval);
            }
        }
    }
    return $retval + 1;
}

# Retrieves the name of the 'subdirectory' where the latest BLASTDBs residue in GCP
sub get_gcs_latest_dir
{
    my $cmd = "/usr/bin/curl -s " . GCS_URL . "/" . GCP_BUCKET . "/latest-dir";
    return `$cmd`;
}

# Fetches the JSON text containing the BLASTDB metadata in GCS
sub get_gcs_blastdb_metadata
{
    my $latest_dir = shift;
    my $url = GCS_URL . "/" . GCP_BUCKET . "/$latest_dir/" . GCP_MANIFEST;
    chomp(my $retval = `/usr/bin/curl -sf $url`);
    return ($retval, $url);
}

# Returns the path to the gsutil utility or undef  if it is not found
sub get_gsutil_path
{
    foreach (qw(/google/google-cloud-sdk/bin /snap/bin /usr/bin)) {
        my $path = "$_/gsutil";
        return $path if (-f $path);
    }
    return undef;
}

# Returns the number of cores, or 1 if unknown
sub get_num_cores
{
    my $retval = 1;
    if ($^O =~ /linux/i) {
        open my $fh, "/proc/cpuinfo" or return $retval;
        $retval = scalar(map /^processor/, &lt;$fh&gt;);
        close($fh);
    } elsif ($^O =~ /darwin/i) {
        chomp($retval = `/usr/sbin/sysctl -n hw.ncpu`);
    }
    return $retval;
}

__END__

=head1 NAME

B&lt;update_blastdb.pl&gt; - Download pre-formatted BLAST databases

=head1 SYNOPSIS

update_blastdb.pl [options] blastdb ...

=head1 OPTIONS

=over 2

=item B&lt;--decompress&gt;

Downloads, decompresses the archives in the current working directory, and
deletes the downloaded archive to save disk space, while preserving the
archive checksum files (default: false).

=item B&lt;--showall&gt;

Show all available pre-formatted BLAST databases (default: false). The output
of this option lists the database names which should be used when
requesting downloads or updates using this script.

It accepts the optional arguments: 'tsv' and 'pretty' to produce tab-separated values
and a human-readable format respectively. These parameters elicit the display of
additional metadata if this is available to the program.
This metadata is displayed in columnar format; the columns represent:

name, description, size in gigabytes, date of last update (YYYY-MM-DD format).

=item B&lt;--blastdb_version&gt;

Specify which BLAST database version to download (default: 4).
Supported values: 4, 5

=item B&lt;--passive&gt;

Use passive FTP, useful when behind a firewall or working in the cloud (default: true).
To disable passive FTP, configure this option as follows: --passive no

=item B&lt;--timeout&gt;

Timeout on connection to NCBI (default: 120 seconds).

=item B&lt;--force&gt;

Force download even if there is a archive already on local directory (default:
false).

=item B&lt;--verbose&gt;

Increment verbosity level (default: 1). Repeat this option multiple times to 
increase the verbosity level (maximum 2).

=item B&lt;--quiet&gt;

Produce no output (default: false). Overrides the B&lt;--verbose&gt; option.

=item B&lt;--version&gt;

Prints this script's version. Overrides all other options.

=item B&lt;--num_cores&gt;

Sets the number of cores to utilize to perform downloads in parallel when data comes from GCS.
Defaults to all cores (Linux and macos only).

=back

=head1 DESCRIPTION

This script will download the pre-formatted BLAST databases requested in the
command line from the NCBI ftp site.

=head1 EXIT CODES

This script returns 0 on successful operations that result in no downloads, 1
on successful operations that downloaded files, and 2 on errors.

=head1 BUGS

Please report them to <a href="mailto:blast-help@ncbi.nlm.nih.gov">&lt;blast-help@ncbi.nlm.nih.gov&gt;</a>

=head1 COPYRIGHT

See PUBLIC DOMAIN NOTICE included at the top of this script.

=cut
</div><div class="clearfix"></div></div></pre><hr>
<div align=center>
   [&nbsp;<b><i>source navigation</i></b>&nbsp;]&nbsp;&nbsp;  [&nbsp;<a href="diff/src/app/blast/update_blastdb.pl">diff markup</a>&nbsp;]&nbsp;&nbsp;  [&nbsp;<a href="ident">identifier search</a>&nbsp;]&nbsp;&nbsp;  [&nbsp;<a href="search">freetext search</a>&nbsp;]&nbsp;&nbsp;  [&nbsp;<a href="find">file search</a>&nbsp;]&nbsp;&nbsp; 
</div>

<hr>
<table width="100%" cellpadding=0 border=0>
  <tr>

  </tr>
  <tr>
    <td align=left>
      This page was automatically generated by the 
      <a href="blurb.html">LXR engine</a>.
      <br>
      Visit the <a href="http://lxr.sf.net/">LXR main site</a> for more information.
    </td>
  </tr>
</table>

</body>
</html>
