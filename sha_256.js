var SHA_256 = (function(){
	"use strict";

	var K = new Uint32Array([
		0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
		0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
		0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
		0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
		0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
		0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
		0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
		0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]);

	function RotR(x,n)  { return (x>>>n)|(x<< (32-n)) }
	function Ch  (x,y,z){ return (x&y)^(~x&z) }
	function Maj (x,y,z){ return (x&y)^(x&z)^(y&z) }
	function S0  (x)    { return RotR(x, 2)^RotR(x,13)^RotR(x,22) }
	function S1  (x)    { return RotR(x, 6)^RotR(x,11)^RotR(x,25) }
	function s0  (x)    { return RotR(x, 7)^RotR(x,18)^(x>>> 3) }
	function s1  (x)    { return RotR(x,17)^RotR(x,19)^(x>>>10) }

	function setMessageBlock(M,msg,blockNum){
		for( var i = 3, j = blockNum*64; i < 64 && j < msg.length; i--, j++ ){
			M[i] = msg.charCodeAt(j);
			if( i%4 === 0 ){
				i += 8;
			}
		}
		if( i < 64 ){
			M[i] = i === 3 && msg.length%64 !== 0 ? 0 : 0x80;
			while( i%4 !== 0 ){
				M[--i] = 0;
			}
			i += 4;
			var isFinalBlock = i+8 <= 64;
			while( i < 64 ){
				M[i++] = 0;
			}
			if( isFinalBlock ){
				var M32 = new Uint32Array(M.buffer);
				M32[15] = msg.length<<3;
				M32[14] = msg.length/0x20000000;
			}
		}
	}

	function buildHash(H){
		for( var i = 0; i < H.length; i++ ){
			var temp = H[i];
			H[i] = (temp<<24)|(temp<<8&0xff0000)|(temp>>>8&0xff00)|(temp>>>24);
		}
		return new Uint8Array(H.buffer);
	}

	function hash(msg){
		if( /[\x80-\xFF]/.test(msg) ){
			msg = unescape(encodeURI(s));
		}
		var H = new Uint32Array([0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]),
		    N = Math.ceil((msg.length+9)/64),
		    W = new Uint32Array(64),
		    M = new Uint8Array(W.buffer,0,64);
		for( var i = 0; i < N; i++ ){
			var a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7], T1, T2;
			setMessageBlock(M,msg,i);
			for( var t = 16; t < 64; t++ ) W[t] = s1(W[t-2]) + W[t-7] + s0(W[t-15]) + W[t-16];
			for( var t =  0; t < 64; t++ ){
				T1 = h + S1(e) + Ch(e,f,g) + K[t] + W[t];
				T2 = S0(a) + Maj(a,b,c);
				h  = g;
				g  = f;
				f  = e;
				e  = d + T1;
				d  = c;
				c  = b;
				b  = a;
				a  = T1 + T2;
			}
			H[0] += a; H[1] += b; H[2] += c; H[3] += d; H[4] += e; H[5] += f; H[6] += g; H[7] += h;
		}
		return buildHash(H);
	}

	return {
		hash    : hash,
		hash_bin: function(msg){ return String.fromCharCode.apply(null,hash(msg))},
		hash_hex: function(msg){ return Array.prototype.map.call(hash(msg),function(x){return (0x100|x).toString(16).slice(1)}).join("")},
		hash_b64: function(msg){ return btoa(String.fromCharCode.apply(null,hash(msg)))}
	};
})();
