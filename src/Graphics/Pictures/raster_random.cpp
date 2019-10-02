
/******************************************************************************
* MODULE     : raster_random.hpp
* DESCRIPTION: Random pictures
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*              Using adapted Perlin filter code from SVG 1.1 specification
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

/* Produces results in the range [1, 2**31 - 2].
Algorithm is: r = (a * r) mod m
where a = 16807 and m = 2**31 - 1 = 2147483647
See [Park & Miller], CACM vol. 31 no. 10 p. 1195, Oct. 1988
To test: the algorithm should produce the result 1043618065
as the 10,000th generated number if the original seed is 1.
*/

#include "raster.hpp"
#include "resource.hpp"
#include "true_color.hpp"

/******************************************************************************
* The perlin ressource
******************************************************************************/

#define BSize 0x100
#define BM 0xff
#define PerlinN 0x1000
#define NP 12 /* 2^PerlinN */
#define NM 0xfff

struct StitchInfo {
  int nWidth; // How much to subtract to wrap for stitching.
  int nHeight;
  int nWrapX; // Minimum value to wrap.
  int nWrapY;
};

RESOURCE(perlin);

struct perlin_rep: rep<perlin> {
  int uLatticeSelector[BSize + BSize + 2];
  double fGradient[4][BSize + BSize + 2][2];
  perlin_rep (string name, long lSeed);
  long setup_seed (long lSeed);
  long random (long lSeed);
  double noise2 (int nColorChannel, double vec[2], StitchInfo *pStitchInfo);
  double turbulence (int nColorChannel,
                     double pointX, double pointY,
                     double fBaseFreqX, double fBaseFreqY,
                     int nNumOctaves, bool bFractalSum, bool bDoStitching,
                     double fTileX, double fTileY,
                     double fTileWidth, double fTileHeight);
};

RESOURCE_CODE(perlin);

perlin
perlin_generator (long lSeed) {
  string name= "perlin-" * as_string (lSeed);
  if (perlin::instances -> contains (name)) return perlin (name);
  return tm_new<perlin_rep> (name, lSeed);
}

/******************************************************************************
* Random number generation
******************************************************************************/

#define RAND_m 2147483647 /* 2**31 - 1 */
#define RAND_a 16807 /* 7**5; primitive root of m */
#define RAND_q 127773 /* m / a */
#define RAND_r 2836 /* m % a */

long
perlin_rep::setup_seed (long lSeed) {
  if (lSeed <= 0) lSeed = -(lSeed % (RAND_m - 1)) + 1;
  if (lSeed > RAND_m - 1) lSeed = RAND_m - 1;
  return lSeed;
}

long
perlin_rep::random (long lSeed) {
  long result;
  result = RAND_a * (lSeed % RAND_q) - RAND_r * (lSeed / RAND_q);
  if (result <= 0) result += RAND_m;
  return result;
}

/******************************************************************************
* Setup random data
******************************************************************************/

perlin_rep::perlin_rep (string name, long lSeed): rep<perlin> (name) {
  double s;
  int i, j, k;
  lSeed = setup_seed(lSeed);
  for (k = 0; k < 4; k++) {
    for (i = 0; i < BSize; i++) {
      uLatticeSelector[i] = i;
      for (j = 0; j < 2; j++)
        fGradient[k][i][j] = (double)(((lSeed = random(lSeed)) % (BSize + BSize)) - BSize) / BSize;
      s = double(sqrt(fGradient[k][i][0] * fGradient[k][i][0] + fGradient[k][i][1] * fGradient[k][i][1]));
      fGradient[k][i][0] /= s;
      fGradient[k][i][1] /= s;
    }
  }
  while (--i) {
    k = uLatticeSelector[i];
    uLatticeSelector[i] = uLatticeSelector[j = (lSeed = random(lSeed)) % BSize];
    uLatticeSelector[j] = k;
  }
  for(i = 0; i < BSize + 2; i++)
  {
    uLatticeSelector[BSize + i] = uLatticeSelector[i];
    for(k = 0; k < 4; k++)
      for(j = 0; j < 2; j++)
        fGradient[k][BSize + i][j] = fGradient[k][i][j];
  }
}

/******************************************************************************
* Generate picture
******************************************************************************/

#define s_curve(t) ( t * t * (3. - 2. * t) )
#define lerp(t, a, b) ( a + t * (b - a) )

double
perlin_rep::noise2 (int nColorChannel, double vec[2],
                    StitchInfo *pStitchInfo) {
  int bx0, bx1, by0, by1, b00, b10, b01, b11;
  double rx0, rx1, ry0, ry1, *q, sx, sy, a, b, t, u, v;
  int i, j;
  t = vec[0] + PerlinN;
  bx0 = (int)t;
  bx1 = bx0+1;
  rx0 = t - (int)t;
  rx1 = rx0 - 1.0f;
  t = vec[1] + PerlinN;
  by0 = (int)t;
  by1 = by0+1;
  ry0 = t - (int)t;
  ry1 = ry0 - 1.0f;
  // If stitching, adjust lattice points accordingly.
  if(pStitchInfo != NULL)
  {
    if(bx0 >= pStitchInfo->nWrapX)
      bx0 -= pStitchInfo->nWidth;
    if(bx1 >= pStitchInfo->nWrapX)
      bx1 -= pStitchInfo->nWidth;
    if(by0 >= pStitchInfo->nWrapY)
      by0 -= pStitchInfo->nHeight;
    if(by1 >= pStitchInfo->nWrapY)
      by1 -= pStitchInfo->nHeight;
  }
  bx0 &= BM;
  bx1 &= BM;
  by0 &= BM;
  by1 &= BM;
  i = uLatticeSelector[bx0];
  j = uLatticeSelector[bx1];
  b00 = uLatticeSelector[i + by0];
  b10 = uLatticeSelector[j + by0];
  b01 = uLatticeSelector[i + by1];
  b11 = uLatticeSelector[j + by1];
  sx = double(s_curve(rx0));
  sy = double(s_curve(ry0));
  q = fGradient[nColorChannel][b00]; u = rx0 * q[0] + ry0 * q[1];
  q = fGradient[nColorChannel][b10]; v = rx1 * q[0] + ry0 * q[1];
  a = lerp(sx, u, v);
  q = fGradient[nColorChannel][b01]; u = rx0 * q[0] + ry1 * q[1];
  q = fGradient[nColorChannel][b11]; v = rx1 * q[0] + ry1 * q[1];
  b = lerp(sx, u, v);
  return lerp(sy, a, b);
}

double
perlin_rep::turbulence (int nColorChannel,
                        double pointX, double pointY,
                        double fBaseFreqX, double fBaseFreqY,
                        int nNumOctaves, bool bFractalSum, bool bDoStitching,
                        double fTileX, double fTileY,
                        double fTileWidth, double fTileHeight) {
  StitchInfo stitch;
  StitchInfo *pStitchInfo = NULL; // Not stitching when NULL.
  // Adjust the base frequencies if necessary for stitching.
  if (bDoStitching) {
    // When stitching tiled turbulence, the frequencies must be adjusted
    // so that the tile borders will be continuous.
    if (fBaseFreqX != 0.0) {
      double fLoFreq = double(floor(fTileWidth * fBaseFreqX)) / fTileWidth;
      double fHiFreq = double(ceil(fTileWidth * fBaseFreqX)) / fTileWidth;
      if(fBaseFreqX / fLoFreq < fHiFreq / fBaseFreqX)
        fBaseFreqX = fLoFreq;
      else
        fBaseFreqX = fHiFreq;
    }
    if(fBaseFreqY != 0.0)
    {
      double fLoFreq = double(floor(fTileHeight * fBaseFreqY)) / fTileHeight;
      double fHiFreq = double(ceil(fTileHeight * fBaseFreqY)) / fTileHeight;
      if(fBaseFreqY / fLoFreq < fHiFreq / fBaseFreqY)
        fBaseFreqY = fLoFreq;
      else
        fBaseFreqY = fHiFreq;
    }
    // Set up initial stitch values.
    pStitchInfo = &stitch;
    stitch.nWidth = int(fTileWidth * fBaseFreqX + 0.5f);
    stitch.nWrapX = fTileX * fBaseFreqX + PerlinN + stitch.nWidth;
    stitch.nHeight = int(fTileHeight * fBaseFreqY + 0.5f);
    stitch.nWrapY = fTileY * fBaseFreqY + PerlinN + stitch.nHeight;
  }
  double fSum = 0.0f;
  double vec[2];
  vec[0] = pointX * fBaseFreqX;
  vec[1] = pointY * fBaseFreqY;
  double ratio = 1;
  for (int nOctave = 0; nOctave < nNumOctaves; nOctave++) {
    if (bFractalSum)
      fSum += double(noise2(nColorChannel, vec, pStitchInfo) / ratio);
    else
      fSum += double(fabs(noise2(nColorChannel, vec, pStitchInfo)) / ratio);
    vec[0] *= 2;
    vec[1] *= 2;
    ratio *= 2;
    if (pStitchInfo != NULL) {
      // Update stitch values. Subtracting PerlinN before the multiplication and
      // adding it afterward simplifies to subtracting it once.
      stitch.nWidth *= 2;
      stitch.nWrapX = 2 * stitch.nWrapX - PerlinN;
      stitch.nHeight *= 2;
      stitch.nWrapY = 2 * stitch.nWrapY - PerlinN;
    }
  }
  return fSum;
}

/******************************************************************************
* Public interface
******************************************************************************/

raster<double>
turbulence (int w, int h, int ox, int oy, long seed,
            double wavelen_x, double wavelen_y, 
            int nNumOctaves, bool bFractalSum) {
  perlin p= perlin_generator (seed);
  raster<double> ret (w, h, ox, oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double v= p->turbulence (3, (double) x, (double) y,
                               1.0 / wavelen_x, 1.0 / wavelen_y,
                               nNumOctaves, bFractalSum, seed<0,
                               0.0, 0.0, wavelen_x, wavelen_y);
      if (bFractalSum) v= (v + 1.0) / 2.0;
      ret->a[y*w+x]= v;
    }
  return ret;
}

raster<double>
turbulence (raster<double> ras, long seed,
            double wavelen_x, double wavelen_y, 
            int nNumOctaves, bool bFractalSum) {
  return turbulence (ras->w, ras->h, ras->ox, ras->oy, seed,
                     wavelen_x, wavelen_y, nNumOctaves, bFractalSum);
}

raster<true_color>
turbulence (raster<true_color> ras, long seed,
            double wavelen_x, double wavelen_y,
            int nNumOctaves, bool bFractalSum) {
  int w= ras->w, h= ras->h, ox= ras->ox, oy= ras->oy;
  perlin p= perlin_generator (seed);
  raster<true_color> ret (w, h, ox, oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double v[4];
      for (int ch=0; ch<4; ch++) {
        v[ch]= p->turbulence (ch, (double) x, (double) y,
                              1.0 / wavelen_x, 1.0 / wavelen_y,
                              nNumOctaves, bFractalSum, seed<0,
                              0.0, 0.0, wavelen_x, wavelen_y);
        if (bFractalSum) v[ch]= (v[ch] + 1.0) / 2.0;
      }
      ret->a[y*w+x]= true_color (v[0], v[1], v[2], v[3]);
    }
  return ret;
}
