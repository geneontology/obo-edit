package org.oboedit.piccolo;
//Decompiled by Jad v1.5.8e. Copyright 2001 Pavel Kouznetsov.
//Jad home page: http://www.geocities.com/kpdus/jad.html
//Decompiler options: packimports(3) 
//Source File Name:   fatlin.java

import java.applet.Applet;
import java.awt.*;
import java.awt.geom.*;

public class fatlin extends Applet
{

 public class Vec2 {
	 public double u;
	 public double v;
	 
	 public Vec2(double u, double v) {
		 this.u = u;
		 this.v = v;
	 }
	}
public Point2D.Double int2dbl(Point p)
 {
     return new Point2D.Double(p.x, p.y);
 }

 public Vec2 root1(int nmdeg, double span, double cf[], double tl, double ur)
 {
     if(cf[0] > 0.0D && cf[nmdeg] < 0.0D)
     {
         double tmax = 0.0D;
         for(int i = 0; i < nmdeg; i++)
             if(cf[i] > 0.0D)
             {
                 for(int j = i + 1; j <= nmdeg; j++)
                     if(cf[j] < cf[i] && cf[j] < 0.0D)
                     {
                         double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                         if(t > tmax)
                             tmax = t;
                     }

             }

         if(tmax != 0.0D)
         {
             if(tl > tmax + 9.9999999999999995E-07D)
                 return new Vec2(-1D, ur);
             if(ur > tmax)
                 ur = tmax;
         }
     } else
     if(cf[0] < 0.0D && cf[nmdeg] > 0.0D)
     {
         double tmin = 1.0D;
         for(int i = 0; i < nmdeg; i++)
             if(cf[i] < 0.0D)
             {
                 for(int j = i + 1; j <= nmdeg; j++)
                     if(cf[j] > cf[i] && cf[j] > 0.0D)
                     {
                         double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                         if(t < tmin)
                             tmin = t;
                     }

             }

         if(tmin != 1.0D)
         {
             if(ur < tmin - 9.9999999999999995E-07D)
                 return new Vec2(-1D, 1.0D);
             if(tl < tmin)
                 tl = tmin;
         }
     } else
     if((cf[0] <= 0.0D || cf[nmdeg] <= 0.0D) && cf[0] < 0.0D && cf[nmdeg] < 0.0D)
     {
         double tmax = 0.0D;
         for(int i = 1; i < nmdeg; i++)
         {
             for(int j = i + 1; j <= nmdeg; j++)
                 if(cf[i] > 0.0D && cf[j] < 0.0D)
                 {
                     double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                     if(t > tmax)
                         tmax = t;
                 }

         }

         if(tmax != 0.0D)
         {
             if(tl > tmax + 9.9999999999999995E-07D)
                 return new Vec2(-1D, 1.0D);
             if(ur > tmax)
                 ur = tmax;
         }
         double tmin = 1.0D;
         for(int i = 0; i < nmdeg - 1; i++)
         {
             for(int j = i + 1; j < nmdeg; j++)
                 if(cf[i] < 0.0D && cf[j] > 0.0D)
                 {
                     double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                     if(t < tmin)
                         tmin = t;
                 }

         }

         if(tmin != 1.0D)
         {
             if(ur < tmin - 9.9999999999999995E-07D)
                 return new Vec2(-1D, 1.0D);
             if(tl < tmin)
                 tl = tmin;
         }
     }
     return new Vec2(tl, ur);
 }

 public Point castljau(double u, int ndeg, Point pnt[])
 {
     double wx[] = new double[10];
     double wy[] = new double[10];
     for(int i = 0; i <= ndeg; i++)
     {
         wx[i] = pnt[i].x;
         wy[i] = pnt[i].y;
     }

     for(int m = 1; m <= ndeg; m++)
     {
         for(int j = 0; j <= ndeg - m; j++)
         {
             wx[j] += (wx[j + 1] - wx[j]) * u;
             wy[j] += (wy[j + 1] - wy[j]) * u;
         }

     }

     return new Point((int)wx[0], (int)wy[0]);
 }

 public void tspli1(double x[], double tl, double ur, int ndeg)
 {
     for(int i = 1; i <= ndeg; i++)
     {
         for(int j = 0; j <= ndeg - i; j++)
             x[j] += tl * (x[j + 1] - x[j]);

     }

     if(ur > 0.99999899999999997D)
         return;
     double tmp = ur / (1.0D - tl);
     for(int i = 1; i <= ndeg; i++)
     {
         for(int j = ndeg; j >= i; j--)
             x[j] += (x[j - 1] - x[j]) * tmp;

     }

 }

 public int croot_sub(double x1[], double t1[], int nint, double xint[], int ndeg, double span, 
         Graphics g)
 {
     double x1l[] = new double[20];
     double x1r[] = new double[20];
     double t1l[] = new double[2];
     double t1r[] = new double[2];
     int jover = 0;
     tdiv(x1, x1l, x1r, 0.5D, ndeg);
     t1l[1] = t1[0] + (t1[1] - t1[0]) * 0.5D;
     t1l[0] = t1[0];
     t1r[0] = t1l[1];
     int iover = curtrm(x1l, t1l, ndeg, span, g);
     if(iover == 1)
     {
         xint[nint] = tintt;
         nint++;
         if(multi == 1)
             return 0;
     } else
     if(iover == -1)
     {
         nint = croot_sub(x1l, t1l, nint, xint, ndeg, span, g);
         if(multi == 1 && nint > 0)
             return 0;
         jover = 1;
     }
     t1r[1] = t1[1];
     iover = curtrm(x1r, t1r, ndeg, span, g);
     if(iover == 1)
     {
         xint[nint] = tintt;
         nint++;
         if(multi == 1)
             return 0;
     } else
     if(iover == -1)
     {
         nint = croot_sub(x1r, t1r, nint, xint, ndeg, span, g);
         jover = 1;
     }
     return nint;
 }

 public void paint(Graphics g)
 {
     int nint = 0;
     int ay = 180;
     int wy = 120;
     int wx = 300;
     double x1[] = new double[10];
     double wt1[] = new double[10];
     double wt2[] = new double[10];
     double xint[] = new double[8];
     g.setColor(Color.yellow);
     for(int i = 0; i < n; i++)
         g.fillOval(p[i].x - 3, p[i].y - 3, 7, 7);

     g.setColor(Color.black);
     for(int i = 1; i < n; i++)
         g.drawLine(p[i - 1].x, p[i - 1].y, p[i].x, p[i].y);

     if(curve1 == 0)
     {
         if(n == ndeg1)
         {
             g.setColor(Color.red);
             for(int i = 0; i < ndeg1; i++)
                 p1[i] = p[i];

             DrawCurve(ndeg1 - 1, p1, g);
             n = 0;
             curve1 = 1;
         }
     } else
     {
         for(int i = 0; i < ndeg1; i++)
             g.fillOval(p1[i].x - 3, p1[i].y - 3, 7, 7);

         g.setColor(Color.black);
         for(int i = 1; i < ndeg1; i++)
             g.drawLine(p1[i - 1].x, p1[i - 1].y, p1[i].x, p1[i].y);

         if(n == mdeg1)
         {
             g.setColor(Color.yellow);
             for(int i = 1; i < mdeg1; i++)
                 g.drawLine(p[i - 1].x, p[i - 1].y, p[i].x, p[i].y);

             if(mdeg1 > 2)
                 DrawCurve(mdeg1 - 1, p, g);
             curve1 = 2;
         }
         g.setColor(Color.red);
         DrawCurve(ndeg1 - 1, p1, g);
     }
     if(curve1 == 2)
     {
         g.setColor(Color.red);
         curve1 = 0;
         for(int i = 0; i < ndeg1; i++)
             Pnt1[i] = int2dbl(p1[i]);

         for(int i = 0; i < mdeg1; i++)
             Pnt2[i] = int2dbl(p[i]);

         if(mdeg1 == 2)
         {
             double a = Pnt2[0].y - Pnt2[1].y;
             double b = Pnt2[1].x - Pnt2[0].x;
             double tmp = Math.sqrt(a * a + b * b);
             a /= tmp;
             b /= tmp;
             double c = -(a * Pnt2[0].x + b * Pnt2[0].y);
             for(int i = 0; i < ndeg1; i++)
                 x1[i] = Pnt1[i].x * a + Pnt1[i].y * b + c;

             nint = croot(x1, xint, ndeg1 - 1, g);
         } else
         {
             for(int i = 0; i < ndeg1; i++)
                 wt1[i] = 1.0D;

             for(int i = 0; i < mdeg1; i++)
                 wt2[i] = 1.0D;

             nint = fatlin2(ndeg1 - 1, Pnt1, wt1, mdeg1 - 1, Pnt2, wt2, g);
         }
         curve1 = 0;
         n = 0;
         g.setColor(Color.white);
         g.drawString("No. of Intersections = " + nint, 20, 35);
         if(nint > 0)
         {
             for(int i = 0; i < nint; i++)
             {
                 if(mdeg1 == 2)
                 {
                     g.drawString(" t = " + xint[i], 25 + 75 * i, 50);
                     p[2] = castljau(xint[i], ndeg1 - 1, p1);
                 } else
                 {
                     g.drawString(" s = " + xint2[i][0], 25 + 75 * i, 50);
                     g.drawString(" t = " + xint2[i][1], 25 + 75 * i, 62);
                     p[2] = castljau(xint2[i][0], ndeg1 - 1, p1);
                 }
                 g.fillOval(p[2].x - 3, p[2].y - 3, 7, 7);
             }

         }
     }
 }

 public int root3(int nmdeg, double span, double cf[], double tlr[])
 {
     if(cf[0] > 0.0D && cf[nmdeg] < 0.0D)
     {
         double tmax = 0.0D;
         for(int i = 0; i < nmdeg; i++)
             if(cf[i] > 0.0D)
             {
                 for(int j = i + 1; j <= nmdeg; j++)
                     if(cf[j] < cf[i] && cf[j] < 0.0D)
                     {
                         double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                         if(t > tmax)
                             tmax = t;
                     }

             }

         if(tmax != 0.0D)
         {
             if(tlr[0] > tmax + 9.9999999999999995E-07D)
                 return -1;
             if(tlr[1] > tmax)
                 tlr[1] = tmax;
         }
     } else
     if(cf[0] < 0.0D && cf[nmdeg] > 0.0D)
     {
         double tmin = 1.0D;
         for(int i = 0; i < nmdeg; i++)
             if(cf[i] < 0.0D)
             {
                 for(int j = i + 1; j <= nmdeg; j++)
                     if(cf[j] > cf[i] && cf[j] > 0.0D)
                     {
                         double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                         if(t < tmin)
                             tmin = t;
                     }

             }

         if(tmin != 1.0D)
         {
             if(tlr[1] < tmin - 9.9999999999999995E-07D)
                 return -1;
             if(tlr[0] < tmin)
                 tlr[0] = tmin;
         }
     } else
     if((cf[0] <= 0.0D || cf[nmdeg] <= 0.0D) && cf[0] < 0.0D && cf[nmdeg] < 0.0D)
     {
         double tmax = 0.0D;
         for(int i = 1; i < nmdeg; i++)
         {
             for(int j = i + 1; j <= nmdeg; j++)
                 if(cf[i] > 0.0D && cf[j] < 0.0D)
                 {
                     double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                     if(t > tmax)
                         tmax = t;
                 }

         }

         if(tmax != 0.0D)
         {
             if(tlr[0] > tmax + 9.9999999999999995E-07D)
                 return -1;
             if(tlr[1] > tmax)
                 tlr[1] = tmax;
         }
         double tmin = 1.0D;
         for(int i = 0; i < nmdeg - 1; i++)
         {
             for(int j = i + 1; j < nmdeg; j++)
                 if(cf[i] < 0.0D && cf[j] > 0.0D)
                 {
                     double t = ((cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i) * span;
                     if(t < tmin)
                         tmin = t;
                 }

         }

         if(tmin != 1.0D)
         {
             if(tlr[1] < tmin - 9.9999999999999995E-07D)
                 return -1;
             if(tlr[0] < tmin)
                 tlr[0] = tmin;
         }
     }
     return 0;
 }

 public void lsplit(int ndeg, double x[], double y[], double ur)
 {
     for(int i = 1; i <= ndeg; i++)
     {
         for(int j = ndeg; j >= i; j--)
         {
             x[j] += (x[j - 1] - x[j]) * ur;
             y[j] += (y[j - 1] - y[j]) * ur;
         }

     }

 }

 public fatlin()
 {
     p = new Point[10];
     p1 = new Point[10];
     Pnt1 = new Point2D.Double[10];
     Pnt2 = new Point2D.Double[10];
     Nc = 4;
     ndiv = 60;
     disp_con = 1;
     TOLEL = 0.01D;
     mdeg1 = 4;
     ndeg1 = 4;
     xint2 = new double[10][4];
 }

 void cuvbuf(int ndeg, double x1[], double y1[], double w1[], double t1[], double xt1[], double yt1[], 
         double wt1[], double tt1[])
 {
     for(int i = 0; i <= ndeg; i++)
     {
         xt1[i] = x1[i];
         yt1[i] = y1[i];
         wt1[i] = w1[i];
     }

     tt1[0] = t1[0];
     tt1[1] = t1[1];
 }

 int s_ovrlp(int ndeg, double x1[], double y1[], double w1[], double t1[], int mdeg, double x2[], 
         double y2[], double w2[], double t2[], int nint, Graphics g)
 {
     double x1l[] = new double[20];
     double y1l[] = new double[20];
     double w1l[] = new double[20];
     double t1l[] = new double[2];
     double x1r[] = new double[20];
     double y1r[] = new double[20];
     double w1r[] = new double[20];
     double t1r[] = new double[2];
     double x2l[] = new double[20];
     double y2l[] = new double[20];
     double w2l[] = new double[20];
     double t2l[] = new double[2];
     double x2r[] = new double[20];
     double y2r[] = new double[20];
     double w2r[] = new double[20];
     double t2r[] = new double[2];
     double xt1[] = new double[20];
     double yt1[] = new double[20];
     double wt1[] = new double[20];
     double tt1[] = new double[2];
     double xt2[] = new double[20];
     double yt2[] = new double[20];
     double wt2[] = new double[20];
     double tt2[] = new double[2];
     int iover = 0;
     iover = ovrlp(ndeg, x1, y1, w1, t1, mdeg, x2, y2, w2, t2, g);
     if(iover == 0);
     if(iover == 1)
     {
         if(nint > 20)
             return -1;
         xint2[nint][0] = t1[0];
         xint2[nint][1] = t2[0];
         nint++;
     } else
     if(iover == -1)
     {
         subdiv(ndeg, x1, y1, w1, x1l, y1l, w1l, x1r, y1r, w1r, 0.5D, t1, t1l, t1r);
         subdiv(mdeg, x2, y2, w2, x2l, y2l, w2l, x2r, y2r, w2r, 0.5D, t2, t2l, t2r);
         cuvbuf(ndeg, x1l, y1l, w1l, t1l, xt1, yt1, wt1, tt1);
         cuvbuf(mdeg, x2l, y2l, w2l, t2l, xt2, yt2, wt2, tt2);
         nint = s_ovrlp(ndeg, x1l, y1l, w1l, t1l, mdeg, x2l, y2l, w2l, t2l, nint, g);
         cuvbuf(ndeg, xt1, yt1, wt1, tt1, x1l, y1l, w1l, t1l);
         cuvbuf(mdeg, xt2, yt2, wt2, tt2, x2l, y2l, w2l, t2l);
         cuvbuf(mdeg, x2r, y2r, w2r, t2r, xt2, yt2, wt2, tt2);
         nint = s_ovrlp(ndeg, x1l, y1l, w1l, t1l, mdeg, x2r, y2r, w2r, t2r, nint, g);
         cuvbuf(mdeg, xt2, yt2, wt2, tt2, x2r, y2r, w2r, t2r);
         cuvbuf(ndeg, x1r, y1r, w1r, t1r, xt1, yt1, wt1, tt1);
         nint = s_ovrlp(ndeg, x1r, y1r, w1r, t1r, mdeg, x2l, y2l, w2l, t2l, nint, g);
         cuvbuf(ndeg, xt1, yt1, wt1, tt1, x1r, y1r, w1r, t1r);
         nint = s_ovrlp(ndeg, x1r, y1r, w1r, t1r, mdeg, x2r, y2r, w2r, t2r, nint, g);
         nsplit = nsplit + 2;
         showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
     } else
     if(iover == -2)
     {
         subdiv(ndeg, x1, y1, w1, x1l, y1l, w1l, x1r, y1r, w1r, 0.5D, t1, t1l, t1r);
         cuvbuf(mdeg, x2, y2, w2, t2, xt2, yt2, wt2, tt2);
         nint = s_ovrlp(ndeg, x1l, y1l, w1l, t1l, mdeg, x2, y2, w2, t2, nint, g);
         cuvbuf(mdeg, xt2, yt2, wt2, tt2, x2, y2, w2, t2);
         nint = s_ovrlp(ndeg, x1r, y1r, w1r, t1r, mdeg, x2, y2, w2, t2, nint, g);
         nsplit++;
         showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
     } else
     if(iover == -3)
     {
         subdiv(mdeg, x2, y2, w2, x2l, y2l, w2l, x2r, y2r, w2r, 0.5D, t2, t2l, t2r);
         cuvbuf(ndeg, x1, y1, w1, t1, xt1, yt1, wt1, tt1);
         nint = s_ovrlp(ndeg, x1, y1, w1, t1, mdeg, x2l, y2l, w2l, t2l, nint, g);
         cuvbuf(ndeg, xt1, yt1, wt1, tt1, x1, y1, w1, t1);
         nint = s_ovrlp(ndeg, x1, y1, w1, t1, mdeg, x2r, y2r, w2r, t2r, nint, g);
         nsplit++;
         showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
     }
     return nint;
 }

 public void DrawCurve(int ndeg, Point pnt[], Graphics g)
 {
     Point old = pnt[0];
     for(int i = 1; i <= ndiv; i++)
     {
         double u = (double)i / (double)ndiv;
         Point current = castljau(u, ndeg, pnt);
         g.drawLine(old.x, old.y, current.x, current.y);
         old = current;
     }

 }

 public int curtrm(double x1[], double t1[], int ndeg, double span, Graphics g)
 {
     double zero = 9.9999999999999995E-07D;
     double cf[] = new double[10];
     for(int i = 0; i <= ndeg; i++)
         cf[i] = x1[i];

     int ic = 0;
     do
     {
         if(cf[0] > zero && cf[ndeg] > zero)
         {
             for(int i = 1; i < ndeg; i++)
                 if(cf[i] < -zero)
                     return -1;

             return 0;
         }
         if(cf[0] < -zero && cf[ndeg] < -zero)
         {
             for(int i = 1; i < ndeg; i++)
                 if(cf[i] > zero)
                     return -1;

             return 0;
         }
         Vec2 TT = root(ndeg, span, cf);
         double t0 = t1[0];
         t1[0] = t0 + (t1[1] - t0) * TT.u;
         t1[1] = t0 + (t1[1] - t0) * TT.v;
         if(t1[1] - t1[0] < TOLEL)
         {
             tintt = (t1[0] + t1[1]) * 0.5D;
             return 1;
         }
         if(TT.u < 0.02D && TT.v > 0.97999999999999998D)
             return -1;
         tspli1(cf, TT.u, 1.0D - TT.v, ndeg);
     } while(++ic < 10);
     return 0;
 }

 int hull(int ndeg, double x[], double y[], double w[], int mdeg, double xx[], double yy[], 
         double ww[], int mono, Graphics g)
 {
     double dmax = 0.0D;
     double cf[] = new double[10];
     double bf[] = new double[10];
     double tlr[] = new double[2];
     int nside = 2;
     double x0 = xx[0];
     double y0 = yy[0];
     double x3 = xx[mdeg];
     double y3 = yy[mdeg];
     double a = y0 - y3;
     double b = x3 - x0;
     double tmp = Math.sqrt(a * a + b * b);
     if(tmp < 9.9999999999999995E-07D)
         tmp = 1.0D;
     a /= tmp;
     b /= tmp;
     double cmin = -a * x0 - b * y0;
     double cmax = cmin;
     for(int i = 1; i < mdeg; i++)
     {
         double tmp1 = -(a * xx[i] + b * yy[i]);
         if(tmp1 < cmax)
             cmax = tmp1;
         if(tmp1 > cmin)
             cmin = tmp1;
     }

     double span = 1.0D / (double)ndeg;
     double tl = 0.0D;
     double ur = 1.0D;
     TU = new Vec2(tl, ur);
     tlr[0] = 0.0D;
     tlr[1] = 1.0D;
     for(int il = 1; il <= nside; il++)
     {
         int inega = 0;
         int iposi = 0;
         if(il == 1)
         {
             for(int k = 0; k <= ndeg; k++)
             {
                 bf[k] = a * x[k] + b * y[k];
                 cf[k] = bf[k] + cmin;
                 if(cf[k] > 0.0001D)
                     iposi = 1;
                 else
                 if(cf[k] < -0.0001D)
                     inega = 1;
             }

         } else
         if(il == 2)
         {
             for(int k = 0; k <= ndeg; k++)
             {
                 cf[k] = -(bf[k] + cmax);
                 if(cf[k] > 0.0001D)
                     iposi = 1;
                 else
                 if(cf[k] < -0.0001D)
                     inega = 1;
             }

         } else
         if(il == 3)
         {
             if(ur - tl < 0.5D)
             {
                 TU = new Vec2(0.0D, 1.0D);
                 return -1;
             }
             double dmin = -b * x0 + a * y0;
             dmax = dmin;
             for(int i = 1; i <= mdeg - 1; i++)
             {
                 tmp = -b * xx[i] + a * yy[i];
                 if(tmp < dmax)
                     dmax = tmp;
                 if(tmp > dmin)
                     dmin = tmp;
             }

             tmp = -b * x3 + a * y3;
             if(tmp < dmax)
                 dmax = tmp;
             if(tmp > dmin)
                 dmin = tmp;
             for(int k = 1; k <= ndeg; k++)
             {
                 cf[k] = (b * x[k] - a * y[k]) + dmin;
                 if(cf[k] > 0.0D)
                     iposi = 1;
                 else
                     inega = 1;
             }

         } else
         {
             for(int k = 1; k <= ndeg; k++)
             {
                 cf[k] = (-b * x[k] + a * y[k]) - dmax;
                 if(cf[k] > 0.0D)
                     iposi = 1;
                 else
                     inega = 1;
             }

         }
         if(iposi == 0)
         {
             TU = new Vec2(0.0D, 1.0D);
             return -1;
         }
         if(inega != 0)
         {
             if(root3(ndeg, span, cf, tlr) < 0)
             {
                 TU = new Vec2(0.0D, 1.0D);
                 return -1;
             }
             TU = new Vec2(tlr[0], tlr[1]);
         } else
         {
             TU = new Vec2(0.0D, 1.0D);
         }
     }

     return 0;
 }

 public boolean mouseDown(Event evt, int x, int y)
 {
     if(n < Nc)
     {
         p[n] = new Point(x, y);
         n++;
     } else
     {
         n = 0;
     }
     repaint();
     return true;
 }

 public boolean action(Event evt, Object obj)
 {
     if(evt.target instanceof Checkbox)
     {
         Checkbox c = (Checkbox)evt.target;
         if(c.getLabel().equals("on"))
         {
             disp_con = 0;
             n = 0;
         } else
         {
             disp_con = 1;
             n = 0;
         }
     }
     if(evt.target instanceof Choice)
     {
         Choice c = (Choice)evt.target;
         n = 0;
         switch(c.getSelectedIndex())
         {
         case 1: // '\001'
             Nc = 4;
             ndeg1 = 4;
             mdeg1 = 2;
             break;

         case 0: // '\0'
             Nc = 4;
             ndeg1 = 4;
             mdeg1 = 4;
             break;

         case 2: // '\002'
             Nc = 5;
             ndeg1 = 5;
             mdeg1 = 4;
             break;

         case 3: // '\003'
             Nc = 6;
             ndeg1 = 6;
             mdeg1 = 4;
             break;

         case 4: // '\004'
             Nc = 7;
             ndeg1 = 7;
             mdeg1 = 4;
             break;

         case 5: // '\005'
             Nc = 8;
             ndeg1 = 8;
             mdeg1 = 4;
             break;

         case 6: // '\006'
             Nc = 9;
             ndeg1 = 9;
             mdeg1 = 4;
             break;
         }
     }
     return true;
 }

 public Vec2 root(int nmdeg, double span, double cf[])
 {
     double tmin = nmdeg;
     double tmax = 0.0D;
     for(int i = 0; i < nmdeg; i++)
         if(cf[i] > 0.0D)
         {
             for(int j = i + 1; j <= nmdeg; j++)
                 if(cf[j] < 0.0D)
                 {
                     double t = (cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i;
                     if(t > tmax)
                         tmax = t;
                     if(t < tmin)
                         tmin = t;
                 }

         } else
         if(cf[i] < 0.0D)
         {
             for(int j = i + 1; j <= nmdeg; j++)
                 if(cf[j] > 0.0D)
                 {
                     double t = (cf[i] / (cf[i] - cf[j])) * (double)(j - i) + (double)i;
                     if(t > tmax)
                         tmax = t;
                     if(t < tmin)
                         tmin = t;
                 }

         }

     double tl = tmin * span;
     double ur = tmax * span;
     return new Vec2(tl, ur);
 }

 public void init()
 {
     setBackground(new Color(160, 160, 170));
     setForeground(Color.red);
     setLayout(new BorderLayout(0, 0));
     CheckboxGroup gc = new CheckboxGroup();
     Checkbox c1 = new Checkbox("on", gc, false);
     Checkbox c2 = new Checkbox("off", gc, true);
     Panel p = new Panel();
     p.setBackground(new Color(40, 40, 145));
     p.add(c1);
     p.add(c2);
     p.add(new Label("Clipped Polygon"));
     add("South", p);
     Choice cn1 = new Choice();
     cn1.addItem("  degree 3 and 3 Bezier curves");
     cn1.addItem("  degree 3 Bezier curve and line");
     cn1.addItem("  degree 4 an 3 Bezier curves");
     cn1.addItem("  degree 5 and 3 Bezier curves");
     cn1.addItem("  degree 6 and 3 Bezier curves");
     cn1.addItem("  degree 7 and 3 Bezier curves");
     add("North", cn1);
     String str;
     if((str = getParameter("dim")) != null)
         Nc = Integer.parseInt(str);
     resize(350, 340);
 }

 public void lr_split(int ndeg, double x[], double y[], double tl, double ur)
 {
     rsplit(ndeg, x, y, tl);
     if(ur == 0.0D)
     {
         return;
     } else
     {
         double tmp = ur / (1.0D - tl);
         lsplit(ndeg, x, y, tmp);
         return;
     }
 }

 public void tdiv(double x[], double xleft[], double xright[], double t, int ndeg)
 {
     double bz[][] = new double[10][10];
     for(int i = 0; i <= ndeg; i++)
         bz[0][i] = x[i];

     for(int k = 0; k < ndeg; k++)
     {
         for(int i = 1; i <= ndeg; i++)
             bz[k + 1][i] = bz[k][i - 1] + t * (bz[k][i] - bz[k][i - 1]);

     }

     for(int i = 0; i <= ndeg; i++)
     {
         xleft[i] = bz[i][i];
         xright[i] = bz[ndeg - i][ndeg];
     }

 }

 void rsplit(int ndeg, double x[], double y[], double tl)
 {
     for(int i = 1; i <= ndeg; i++)
     {
         for(int j = 0; j <= ndeg - i; j++)
         {
             x[j] += (x[j + 1] - x[j]) * tl;
             y[j] += (y[j + 1] - y[j]) * tl;
         }

     }

 }

 int fatlin2(int ndeg, Point2D.Double p1[], double wt1[], int mdeg, Point2D.Double p[], double wt2[], Graphics g)
 {
     double t1[] = new double[2];
     double t2[] = new double[2];
     double x1[] = new double[20];
     double x2[] = new double[20];
     double y1[] = new double[20];
     double y2[] = new double[20];
     double w1[] = new double[20];
     double w2[] = new double[20];
     for(int i = 0; i <= ndeg; i++)
     {
         x1[i] = p1[i].x * wt1[i];
         y1[i] = p1[i].y * wt1[i];
         w1[i] = wt1[i];
     }

     for(int i = 0; i <= mdeg; i++)
     {
         x2[i] = p[i].x * wt2[i];
         y2[i] = p[i].y * wt2[i];
         w2[i] = wt2[i];
     }

     nsplit = 0;
     int nint = 0;
     n_clip = 0;
     t1[0] = 0.0D;
     t1[1] = 1.0D;
     t2[0] = 0.0D;
     t2[1] = 1.0D;
     nint = s_ovrlp(ndeg, x1, y1, w1, t1, mdeg, x2, y2, w2, t2, nint, g);
     return nint;
 }

 public void DispDiv(int ndeg, Graphics g)
 {
     double x[] = new double[10];
     double y[] = new double[10];
     double t = 0.5D;
     int prx[] = new int[10];
     int pry[] = new int[10];
     int plx[] = new int[10];
     int ply[] = new int[10];
     double xleft[] = new double[10];
     double xright[] = new double[10];
     for(int i = 0; i <= ndeg; i++)
         x[i] = p[i].x;

     tdiv(x, xleft, xright, t, ndeg);
     for(int i = 0; i <= ndeg; i++)
         plx[i] = (int)xleft[i];

     for(int i = 0; i <= ndeg; i++)
         prx[i] = (int)xright[i];

     for(int i = 0; i <= ndeg; i++)
         y[i] = p[i].y;

     tdiv(y, xleft, xright, t, ndeg);
     for(int i = 0; i <= ndeg; i++)
         ply[i] = (int)xleft[i];

     for(int i = 0; i <= ndeg; i++)
         pry[i] = (int)xright[i];

     g.setColor(Color.white);
     for(int i = 1; i <= ndeg; i++)
         g.drawLine(plx[i - 1], ply[i - 1], plx[i], ply[i]);

     g.setColor(Color.gray);
     for(int i = 1; i <= ndeg; i++)
         g.drawLine(prx[i - 1], pry[i - 1], prx[i], pry[i]);

 }

 int ovrlp(int ndeg, double x1[], double y1[], double w1[], double t1[], int mdeg, double x2[], 
         double y2[], double w2[], double t2[], Graphics g)
 {
     double aa2 = 0.0D;
     double bb2 = 1.0D;
     double aa = 0.0D;
     double bb = 1.0D;
     int jover = 0;
     int icount = 0;
     do
     {
         iproces++;
         int mono1 = 0;
         int mono2 = 0;
         jover = hull(mdeg, x2, y2, w2, ndeg, x1, y1, w1, mono1, g);
         if(jover < 0)
             return 0;
         if(jover == 1)
             return -3;
         double ur22 = 1.0D - TU.v;
         double tl2 = TU.u;
         double ur2 = TU.v;
         double tmp2 = t2[0] + tl2 * (t2[1] - t2[0]);
         double tmp1 = t2[1] + ur22 * (t2[0] - t2[1]);
         if(tmp1 - tmp2 < TOLEL && t1[1] - t1[0] <= TOLEL)
         {
             t2[1] = tmp1;
             t2[0] = tmp2;
             return 1;
         }
         if(tl2 > 0.02D || ur2 < 0.97999999999999998D)
         {
             t2[1] = tmp1;
             t2[0] = tmp2;
             lr_split(mdeg, x2, y2, tl2, ur22);
             n_clip++;
             showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
             if(disp_con == 0)
             {
                 g.setColor(Color.green);
                 for(int k = 0; k < mdeg; k++)
                     g.drawLine((int)x2[k], (int)y2[k], (int)x2[k + 1], (int)y2[k + 1]);

             }
         }
         jover = hull(ndeg, x1, y1, w1, mdeg, x2, y2, w2, mono2, g);
         if(jover < 0)
             return 0;
         if(jover == 1)
             return -2;
         double tl1 = TU.u;
         double ur1 = TU.v;
         if(ur2 - tl2 > 0.80000000000000004D && ur1 - tl1 > 0.80000000000000004D)
             return -1;
         if(ur1 - tl1 > 0.97999999999999998D)
             return -2;
         double ur11 = 1.0D - ur1;
         tmp1 = t1[0] + tl1 * (t1[1] - t1[0]);
         tmp2 = t1[1] + ur11 * (t1[0] - t1[1]);
         if(ur2 - tl2 > 0.97999999999999998D && icount > 0)
         {
             t1[1] = tmp2;
             t1[0] = tmp1;
             lr_split(ndeg, x1, y1, tl1, ur11);
             n_clip++;
             showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
             if(disp_con == 0)
             {
                 g.setColor(Color.gray);
                 g.drawLine((int)x1[0], (int)y1[0], (int)x1[ndeg], (int)y1[ndeg]);
             }
             return -3;
         }
         if(tmp2 - tmp1 <= TOLEL && t2[1] - t2[0] < TOLEL)
         {
             t1[1] = tmp2;
             t1[0] = tmp1;
             return 1;
         }
         t1[1] = tmp2;
         t1[0] = tmp1;
         lr_split(ndeg, x1, y1, tl1, ur11);
         n_clip++;
         showStatus(" Number of split=" + nsplit + " Number of clip=" + n_clip);
         if(disp_con == 0)
         {
             g.setColor(Color.gray);
             for(int k = 0; k < ndeg; k++)
                 g.drawLine((int)x1[k], (int)y1[k], (int)x1[k + 1], (int)y1[k + 1]);

         }
     } while(++icount < 20);
     return 0;
 }

 public void subdiv(int ndeg, double x[], double y[], double w[], double xl[], double yl[], double wl[], 
         double xr[], double yr[], double wr[], double t, double t0[], double tl[], 
         double tr[])
 {
     double bx[][] = new double[20][20];
     double by[][] = new double[20][20];
     double bw[][] = new double[20][20];
     for(int i = 0; i <= ndeg; i++)
     {
         bx[0][i] = x[i];
         by[0][i] = y[i];
     }

     for(int k = 0; k <= ndeg; k++)
     {
         for(int i = 1; i <= ndeg; i++)
         {
             bx[k + 1][i] = bx[k][i - 1] + t * (bx[k][i] - bx[k][i - 1]);
             by[k + 1][i] = by[k][i - 1] + t * (by[k][i] - by[k][i - 1]);
         }

     }

     for(int i = 0; i <= ndeg; i++)
     {
         xl[i] = bx[i][i];
         yl[i] = by[i][i];
         int ii = ndeg - i;
         xr[i] = bx[ii][ndeg];
         yr[i] = by[ii][ndeg];
     }

     tl[0] = t0[0];
     tl[1] = t0[0] + (t0[1] - t0[0]) * t;
     tr[0] = tl[1];
     tr[1] = t0[1];
 }

 public int croot(double x1[], double xint[], int ndeg, Graphics g)
 {
     double t1[] = new double[2];
     int nint = 0;
     t1[0] = 0.0D;
     t1[1] = 1.0D;
     double span = 1.0D / (double)ndeg;
     int iover = curtrm(x1, t1, ndeg, span, g);
     if(iover == 1)
     {
         xint[nint] = tintt;
         return 1;
     }
     if(iover == -1)
     {
         if(t1[1] - t1[0] < 0.97999999999999998D)
             tspli1(x1, t1[0], 1.0D - t1[1], ndeg);
         nint = croot_sub(x1, t1, nint, xint, ndeg, span, g);
     }
     return nint;
 }

 Point p[];
 Point p1[];
 Point2D.Double Pnt1[];
 Point2D.Double Pnt2[];
 int n;
 int Nc;
 int ndiv;
 int disp_con;
 int curve1;
 double TOLEL;
 int multi;
 double tintt;
 int nsplit;
 int iproces;
 int n_clip;
 int mdeg1;
 int ndeg1;
 double xint2[][];
 Vec2 TU;
}
