var SHA_256 = (function(){
	
	// polyfill for reversing typed arrays
    if( !Uint8Array.prototype.reverse ){
        Uint8Array.prototype.reverse = Uint32Array.prototype.reverse = function(){
            for( var i = 0; i < this.length/2; i++ ){
                var temp = this[i];
                this[i] = this[this.length-1-i];
                this[this.length-1-i] = temp;
            }
            return this;
        };
    }

    var K = new Uint32Array([
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]);

    function RotL(x,n)  { return (x<< n)|(x>>>(32-n)) }
    function RotR(x,n)  { return (x>>>n)|(x<< (32-n)) }
    function Ch  (x,y,z){ return (x&y)^(~x&z) }
    function Maj (x,y,z){ return (x&y)^(x&z)^(y&z) }
    function S0  (x)    { return RotR(x, 2)^RotR(x,13)^RotR(x,22) }
    function S1  (x)    { return RotR(x, 6)^RotR(x,11)^RotR(x,25) }
    function s0  (x)    { return RotR(x, 7)^RotR(x,18)^(x>>> 3) }
    function s1  (x)    { return RotR(x,17)^RotR(x,19)^(x>>>10) }

    function utf8ToAscii(s){return unescape(encodeURI(s))}
    function toByteArray(H){return new Uint8Array(H.reverse().buffer).reverse()}
    function toBin(b){return String.fromCharCode.apply(null,b)}
    function toHex(b){return Array.prototype.slice.call(b).map(function(x){return (x|0x100).toString(16).substr(1)}).join("")}
    function toB64(b){return btoa(String.fromCharCode.apply(null,b))}

    function hash(msg){
        var H = new Uint32Array([0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]);

        msg = utf8ToAscii(msg);
        var N = Math.ceil((msg.length+9)/64),
            b = new Uint8Array(N*64);
        for( var i = 0, j = b.length-1; i < msg.length; i++, j-- ) b[j] = msg.charCodeAt(i);
        b[j] = 0x80;
        var M = new Uint32Array(b.buffer).reverse();
        M[M.length-1] = (msg.length&0x1fffffff)<<3;
        M[M.length-2] = msg.length/0x20000000;

        for( var i = 0; i < N; i++ ){
            var W = new Uint32Array(64), a = H[0], b = H[1], c = H[2], d = H[3], e = H[4], f = H[5], g = H[6], h = H[7], T1, T2;
            for( var t =  0; t < 16; t++ ) W[t] = M[i*16+t];
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
        return toByteArray(H);
    }

    return {
        hash    : hash,
        hash_bin: function(msg){ return toBin(hash(msg))},
        hash_hex: function(msg){ return toHex(hash(msg))},
        hash_b64: function(msg){ return toB64(hash(msg))}
    };
})();
