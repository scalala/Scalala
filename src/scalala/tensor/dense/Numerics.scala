/*
 * Distributed as part of Scalala, a linear algebra library.
 * 
 * Copyright (C) 2008- Daniel Ramage
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110 USA 
 */
package scalala.tensor.dense;

/**
 * Interface to BLAS / LAPack based on the MTJ implementation, based on
 * a direct translation from MTJ.
 * 
 * @author dramage
 * @author (original) Bjørn-Ove Heimsund
 */
object Numerics {

  /*
  val blas = {
    try {
      NNI_BLASkernel();
    } catch {
      case _ => JLAPACK_BLASkernel();
    }
  }
  */
  
  /*
  val lapack = {
    try {
      NNI_LAPAKkernel();
    } catch {
      case _ => JLAPACK_LAPACKkernel();
    }
  }
  */
  
  val blas   = new JLAPACK_BLASkernel();
  val lapack = new JLAPACK_LAPACKkernel();
}

class JLAPACK_LAPACKkernel {
  import org.netlib.lapack._;
  import org.netlib.util.{doubleW, intW};
  
  def lamch(cmach : String) =
    Dlamch.dlamch(cmach);
  
  def laenv(ispec : Int, name : String, opts : String, n1 : Int, n2 : Int, n3 : Int, n4 : Int) =
    Ilaenv.ilaenv(ispec, name, opts, n1, n2, n3, n4);

  /*
    public int gesvd(JobSVD jobu, JobSVD jobvt, int m, int n, double[] A,
            double[] s, double[] U, double[] Vt, double[] work, int lwork) {
        intW info = new intW(0);
        Dgesvd.dgesvd(jobSVD(jobu), jobSVD(jobvt), m, n, A, 0, ld(m), s, 0, U,
                0, ld(m), Vt, 0, ld(n), work, 0, lwork, info);
        return info.val;
    }

    public int gesdd(JobSVD jobz, int m, int n, double[] A, double[] s,
            double[] U, double[] Vt, double[] work, int lwork, int[] iwork) {
        intW info = new intW(0);
        Dgesdd.dgesdd(jobSVD(jobz), m, n, A, 0, ld(m), s, 0, U, 0, ld(m), Vt,
                0, ld(n), work, 0, lwork, iwork, 0, info);
        return info.val;
    }

    public int gelss(int m, int n, int nrhs, double[] A, double[] B,
            double[] s, double rcond, int[] rank, double[] work, int lwork) {
        intW info = new intW(0);
        intW rankW = new intW(rank[0]);
        Dgelss.dgelss(m, n, nrhs, A, 0, ld(m), B, 0, ld(m, n), s, 0, rcond,
                rankW, work, 0, lwork, info);
        rank[0] = rankW.val;
        return info.val;
    }

    public int gelsd(int m, int n, int nrhs, double[] A, double[] B,
            double[] s, double rcond, int[] rank, double[] work, int lwork,
            int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond);
        intW rankW = new intW(rank[0]);
        Dgelsd.dgelsd(m, n, nrhs, A, 0, ld(m), B, 0, ld(m, n), s, 0, rcondW,
                rankW, work, 0, lwork, iwork, 0, info);
        rank[0] = rankW.val;
        return info.val;
    }
*/

    sealed trait JobEig { def repr: String }
    case object Eigenvalues  extends JobEig{ def repr = "N" };
    case object EigenvaluesAndVectors  extends JobEig{ def repr = "V" };

    def geev(jobvl: JobEig, jobvr: JobEig, n: Int, A: Array[Double], wr: Array[Double],
             wi: Array[Double], Vl: Array[Double], Vr: Array[Double], work: Array[Double], lwork: Int)  = {
        val info = new intW(0);
        Dgeev.dgeev(jobvl.repr, jobvr.repr, n, A, 0, ld(n), wr, 0, wi, 0,
                Vl, 0, ld(n), Vr, 0, ld(n), work, 0, lwork, info);
        info.`val`;
    }
/*
    public int syev(JobEig jobz, UpLo uplo, int n, double[] A, double[] w,
            double[] work, int lwork) {
        intW info = new intW(0);
        Dsyev.dsyev(jobEig(jobz), uplo(uplo), n, A, 0, ld(n), w, 0, work, 0,
                lwork, info);
        return info.val;
    }

    public int spev(JobEig jobz, UpLo uplo, int n, double[] Ap, double[] w,
            double[] Z, double[] work) {
        intW info = new intW(0);
        Dspev.dspev(jobEig(jobz), uplo(uplo), n, Ap, 0, w, 0, Z, 0, ld(n),
                work, 0, info);
        return info.val;
    }

    public int sbev(JobEig jobz, UpLo uplo, int n, int kd, double[] Ab,
            double[] w, double[] Z, double[] work) {
        intW info = new intW(0);
        Dsbev.dsbev(jobEig(jobz), uplo(uplo), n, kd, Ab, 0, ld(kd + 1), w, 0,
                Z, 0, ld(n), work, 0, info);
        return info.val;
    }

    public int stev(JobEig jobz, int n, double[] d, double[] e, double[] Z,
            double[] work) {
        intW info = new intW(0);
        Dstev.dstev(jobEig(jobz), n, d, 0, e, 0, Z, 0, ld(n), work, 0, info);
        return info.val;
    }
   */

    def gels(transpose : Boolean, m : Int, n : Int, nrhs : Int, A : Array[Double],
            B : Array[Double], work : Array[Double], lwork : Int) = {
        val info = new intW(0);
        Dgels.dgels(trans(transpose), m, n, nrhs, A, 0, ld(m), B, 0, ld(m, n),
                work, 0, lwork, info);
        info.`val`;
    }

    def gesv(n : Int, nrhs : Int, A : Array[Double], ipiv : Array[Int], B : Array[Double]) = {
        val info = new intW(0);
        Dgesv.dgesv(n, nrhs, A, 0, ld(n), ipiv, 0, B, 0, ld(n), info);
        info.`val`;
    }

    /*
    public int gbsv(int n, int kl, int ku, int nrhs, double[] Ab, int[] ipiv,
            double[] B) {
        intW info = new intW(0);
        Dgbsv.dgbsv(n, kl, ku, nrhs, Ab, 0, ld(2 * kl + ku + 1), ipiv, 0, B, 0,
                ld(n), info);
        return info.val;
    }

    public int gtsv(int n, int nrhs, double[] dl, double[] d, double[] du,
            double[] B) {
        intW info = new intW(0);
        Dgtsv.dgtsv(n, nrhs, dl, 0, d, 0, du, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int posv(UpLo uplo, int n, int nrhs, double[] A, double[] B) {
        intW info = new intW(0);
        Dposv.dposv(uplo(uplo), n, nrhs, A, 0, ld(n), B, 0, ld(n), info);
        return info.val;
    }

    public int ppsv(UpLo uplo, int n, int nrhs, double[] Ap, double[] B) {
        intW info = new intW(0);
        Dppsv.dppsv(uplo(uplo), n, nrhs, Ap, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int pptrf(UpLo uplo, int n, double[] Ap) {
        intW info = new intW(0);
        Dpptrf.dpptrf(uplo(uplo), n, Ap, 0, info);
        return info.val;
    }

    public int pptrs(UpLo uplo, int n, int nrhs, double[] Ap, double[] B) {
        intW info = new intW(0);
        Dpptrs.dpptrs(uplo(uplo), n, nrhs, Ap, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int ppcon(UpLo uplo, int n, double[] Ap, double anorm,
            double[] rcond, double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dppcon.dppcon(uplo(uplo), n, Ap, 0, anorm, rcondW, work, 0, iwork, 0,
                info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public int pbsv(UpLo uplo, int n, int kd, int nrhs, double[] Ab, double[] B) {
        intW info = new intW(0);
        Dpbsv.dpbsv(uplo(uplo), n, kd, nrhs, Ab, 0, ld(kd + 1), B, 0, ld(n),
                info);
        return info.val;
    }

    public int pbtrf(UpLo uplo, int n, int kd, double[] Ab) {
        intW info = new intW(0);
        Dpbtrf.dpbtrf(uplo(uplo), n, kd, Ab, 0, ld(kd + 1), info);
        return info.val;
    }

    public int pbtrs(UpLo uplo, int n, int kd, int nrhs, double[] Ab, double[] B) {
        intW info = new intW(0);
        Dpbtrs.dpbtrs(uplo(uplo), n, kd, nrhs, Ab, 0, ld(kd + 1), B, 0, ld(n),
                info);
        return info.val;
    }

    public int pbcon(UpLo uplo, int n, int kd, double[] Ab, double anorm,
            double[] rcond, double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dpbcon.dpbcon(uplo(uplo), n, kd, Ab, 0, ld(kd + 1), anorm, rcondW,
                work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public int ptsv(int n, int nrhs, double[] d, double[] e, double[] B) {
        intW info = new intW(0);
        Dptsv.dptsv(n, nrhs, d, 0, e, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int sysv(UpLo uplo, int n, int nrhs, double[] A, int[] ipiv,
            double[] B, double[] work, int lwork) {
        intW info = new intW(0);
        Dsysv.dsysv(uplo(uplo), n, nrhs, A, 0, ld(n), ipiv, 0, B, 0, ld(n),
                work, 0, lwork, info);
        return info.val;
    }

    public int spsv(UpLo uplo, int n, int nrhs, double[] Ap, int[] ipiv,
            double[] B) {
        intW info = new intW(0);
        Dspsv.dspsv(uplo(uplo), n, nrhs, Ap, 0, ipiv, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int geqrf(int m, int n, double[] A, double[] tau, double[] work,
            int lwork) {
        intW info = new intW(0);
        Dgeqrf.dgeqrf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int ormqr(Side side, Transpose trans, int m, int n, int k,
            double[] A, double[] tau, double[] C, double[] work, int lwork) {
        intW info = new intW(0);
        Dormqr.dormqr(side(side), trans(trans), m, n, k, A, 0,
                side == Side.Left ? ld(m) : ld(n), tau, 0, C, 0, ld(m), work,
                0, lwork, info);
        return info.val;
    }

    public int orgqr(int m, int n, int k, double[] A, double[] tau,
            double[] work, int lwork) {
        intW info = new intW(0);
        Dorgqr.dorgqr(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int geqlf(int m, int n, double[] A, double[] tau, double[] work,
            int lwork) {
        intW info = new intW(0);
        Dgeqlf.dgeqlf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int ormql(Side side, Transpose trans, int m, int n, int k,
            double[] A, double[] tau, double[] C, double[] work, int lwork) {
        intW info = new intW(0);
        Dormql.dormql(side(side), trans(trans), m, n, k, A, 0,
                side == Side.Left ? ld(m) : ld(n), tau, 0, C, 0, ld(m), work,
                0, lwork, info);
        return info.val;
    }

    public int orgql(int m, int n, int k, double[] A, double[] tau,
            double[] work, int lwork) {
        intW info = new intW(0);
        Dorgql.dorgql(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int gerqf(int m, int n, double[] A, double[] tau, double[] work,
            int lwork) {
        intW info = new intW(0);
        Dgerqf.dgerqf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int ormrq(Side side, Transpose trans, int m, int n, int k,
            double[] A, double[] tau, double[] C, double[] work, int lwork) {
        intW info = new intW(0);
        Dormrq.dormrq(side(side), trans(trans), m, n, k, A, 0, ld(k), tau, 0,
                C, 0, ld(m), work, 0, lwork, info);
        return info.val;
    }

    public int orgrq(int m, int n, int k, double[] A, double[] tau,
            double[] work, int lwork) {
        intW info = new intW(0);
        Dorgrq.dorgrq(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int gelqf(int m, int n, double[] A, double[] tau, double[] work,
            int lwork) {
        intW info = new intW(0);
        Dgelqf.dgelqf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int ormlq(Side side, Transpose trans, int m, int n, int k,
            double[] A, double[] tau, double[] C, double[] work, int lwork) {
        intW info = new intW(0);
        Dormlq.dormlq(side(side), trans(trans), m, n, k, A, 0, ld(k), tau, 0,
                C, 0, ld(m), work, 0, lwork, info);
        return info.val;
    }

    public int orglq(int m, int n, int k, double[] A, double[] tau,
            double[] work, int lwork) {
        intW info = new intW(0);
        Dorglq.dorglq(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public int gbtrf(int m, int n, int kl, int ku, double[] Ab, int[] ipiv) {
        intW info = new intW(0);
        Dgbtrf.dgbtrf(m, n, kl, ku, Ab, 0, 2 * kl + ku + 1, ipiv, 0, info);
        return info.val;
    }

    public int gbtrs(Transpose trans, int n, int kl, int ku, int nrhs,
            double[] Ab, int[] ipiv, double[] B) {
        intW info = new intW(0);
        Dgbtrs.dgbtrs(trans(trans), n, kl, ku, nrhs, Ab, 0, 2 * kl + ku + 1,
                ipiv, 0, B, 0, ld(n), info);
        return info.val;
    }

    public int getrf(int m, int n, double[] A, int[] ipiv) {
        intW info = new intW(0);
        Dgetrf.dgetrf(m, n, A, 0, ld(m), ipiv, 0, info);
        return info.val;
    }

    public int getrs(Transpose trans, int n, int nrhs, double[] A, int[] ipiv,
            double[] B) {
        intW info = new intW(0);
        Dgetrs.dgetrs(trans(trans), n, nrhs, A, 0, ld(n), ipiv, 0, B, 0, ld(n),
                info);
        return info.val;
    }

    public int gecon(Norm norm, int n, double[] A, double anorm,
            double[] rcond, double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgecon.dgecon(norm(norm), n, A, 0, ld(n), anorm, rcondW, work, 0,
                iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public int gbcon(Norm norm, int n, int kl, int ku, double[] Ab, int[] ipiv,
            double anorm, double[] rcond, double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgbcon.dgbcon(norm(norm), n, kl, ku, Ab, 0, ld(2 * kl + ku + 1), ipiv,
                0, anorm, rcondW, work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public int gttrf(int n, double[] dl, double[] d, double[] du, double[] du2,
            int[] ipiv) {
        intW info = new intW(0);
        Dgttrf.dgttrf(n, dl, 0, d, 0, du, 0, du2, 0, ipiv, 0, info);
        return info.val;
    }

    public int gttrs(Transpose trans, int n, int nrhs, double[] dl, double[] d,
            double[] du, double[] du2, int[] ipiv, double[] B) {
        intW info = new intW(0);
        Dgttrs.dgttrs(trans(trans), n, nrhs, dl, 0, d, 0, du, 0, du2, 0, ipiv,
                0, B, 0, ld(n), info);
        return info.val;
    }

    public int gtcon(Norm norm, int n, double[] dl, double[] d, double[] du,
            double[] du2, int[] ipiv, double anorm, double[] rcond,
            double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgtcon.dgtcon(norm(norm), n, dl, 0, d, 0, du, 0, du2, 0, ipiv, 0,
                anorm, rcondW, work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public int trtrs(UpLo uplo, Transpose trans, Diag diag, int n, int nrhs,
            double[] A, int lda, double[] B) {
        intW info = new intW(0);
        Dtrtrs.dtrtrs(uplo(uplo), trans(trans), diag(diag), n, nrhs, A, 0, lda,
                B, 0, ld(n), info);
        return info.val;
    }

    public int tptrs(UpLo uplo, Transpose trans, Diag diag, int n, int nrhs,
            double[] Ap, double[] B) {
        intW info = new intW(0);
        Dtptrs.dtptrs(uplo(uplo), trans(trans), diag(diag), n, nrhs, Ap, 0, B,
                0, ld(n), info);
        return info.val;
    }

    public int tbtrs(UpLo uplo, Transpose trans, Diag diag, int n, int kd,
            int nrhs, double[] Ab, double[] B) {
        intW info = new intW(0);
        Dtbtrs.dtbtrs(uplo(uplo), trans(trans), diag(diag), n, kd, nrhs, Ab, 0,
                ld(kd + 1), B, 0, ld(n), info);
        return info.val;
    }
    */

    def potrf(uplo: String, n: Int, A: Array[Double]) = {
        val info = new intW(0);
        Dpotrf.dpotrf(uplo, n, A, 0, ld(n), info);
        info.`val`;
    }
    /*

    public int potrs(UpLo uplo, int n, int nrhs, double[] A, double[] B) {
        intW info = new intW(0);
        Dpotrs.dpotrs(uplo(uplo), n, nrhs, A, 0, ld(n), B, 0, ld(n), info);
        return info.val;
    }

    public int pocon(UpLo uplo, int n, double[] A, double anorm,
            double[] rcond, double[] work, int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dpocon.dpocon(uplo(uplo), n, A, 0, ld(n), anorm, rcondW, work, 0,
                iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }
    */

    private def ld(n : Int) =
      Math.max(1,n);

    private def ld(m : Int, n : Int) =
        Math.max(1, Math.max(m, n));

    private def trans(trans : Boolean) =
      if (trans) "T" else "N";

    /*
    private String side(Side side) {
        if (side == Side.Left)
            return "L";
        else
            return "R";
    }

    private String uplo(UpLo uplo) {
        if (uplo == UpLo.Lower)
            return "L";
        else
            return "U";
    }

    private String diag(Diag diag) {
        if (diag == Diag.NonUnit)
            return "N";
        else
            return "U";
    }

    private String jobEig(JobEig job) {
        if (job == JobEig.Eigenvalues)
            return "N";
        else
            return "V";
    }

    private String jobEigRange(JobEigRange job) {
        if (job == JobEigRange.All)
            return "A";
        else if (job == JobEigRange.Interval)
            return "V";
        else
            return "I";
    }

    private String jobSVD(JobSVD job) {
        if (job == JobSVD.All)
            return "A";
        else if (job == JobSVD.Part)
            return "S";
        else if (job == JobSVD.Overwrite)
            return "O";
        else
            return "N";
    }

    private String norm(Norm norm) {
        if (norm == Norm.One)
            return "1";
        else if (norm == Norm.Infinity)
            return "I";
        else
            throw new IllegalArgumentException(
                    "Norm must be the 1 or the Infinity norm");
    }

    public int sbevd(JobEig jobz, UpLo uplo, int n, int kd, double[] Ab,
            double[] w, double[] Z, double[] work, int lwork, int[] iwork,
            int liwork) {
        intW info = new intW(0);
        Dsbevd.dsbevd(jobEig(jobz), uplo(uplo), n, kd, Ab, 0, ld(kd + 1), w, 0,
                Z, 0, ld(n), work, 0, lwork, iwork, 0, liwork, info);
        return info.val;
    }

    public int spevd(JobEig jobz, UpLo uplo, int n, double[] Ap, double[] w,
            double[] Z, double[] work, int lwork, int[] iwork, int liwork) {
        intW info = new intW(0);
        Dspevd.dspevd(jobEig(jobz), uplo(uplo), n, Ap, 0, w, 0, Z, 0, ld(n),
                work, 0, lwork, iwork, 0, liwork, info);
        return info.val;
    }

    public int stevr(JobEig jobz, JobEigRange range, int n, double[] d,
            double[] e, double vl, double vu, int il, int iu, double abstol,
            int[] m, double[] w, double[] Z, int[] isuppz, double[] work,
            int lwork, int[] iwork, int liwork) {
        intW info = new intW(0);
        intW mW = new intW(0);
        Dstevr.dstevr(jobEig(jobz), jobEigRange(range), n, d, 0, e, 0, vl, vu,
                il, iu, abstol, mW, w, 0, Z, 0, ld(n), isuppz, 0, work, 0,
                lwork, iwork, 0, liwork, info);
        m[0] = mW.val;
        return info.val;
    }

    public int syevr(JobEig jobz, JobEigRange range, UpLo uplo, int n,
            double[] A, double vl, double vu, int il, int iu, double abstol,
            int[] m, double[] w, double[] Z, int[] isuppz, double[] work,
            int lwork, int[] iwork, int liwork) {
        intW info = new intW(0);
        intW mW = new intW(0);
        Dsyevr.dsyevr(jobEig(jobz), jobEigRange(range), uplo(uplo), n, A, 0,
                ld(n), vl, vu, il, iu, abstol, mW, w, 0, Z, 0, ld(n), isuppz,
                0, work, 0, lwork, iwork, 0, liwork, info);
        m[0] = mW.val;
        return info.val;
    }
    */
}

class JLAPACK_BLASkernel() {
  import org.netlib.blas._;

  def dot(N : Int, X : Array[Double], Y : Array[Double]) =
    Ddot.ddot(N, X, 0, 1, Y, 0, 1);

  /*
    public double nrm2(int N, double[] X) {
        return Dnrm2.dnrm2(N, X, 0, 1);
    }

    public double asum(int N, double[] X) {
        return Dasum.dasum(N, X, 0, 1);
    }

    public int idamax(int N, double[] X) {
        return Idamax.idamax(N, X, 0, 1);
    }

    public void swap(int N, double[] X, double[] Y) {
        Dswap.dswap(N, X, 0, 1, Y, 0, 1);
    }

    public void copy(int N, double[] X, double[] Y) {
        Dcopy.dcopy(N, X, 0, 1, Y, 0, 1);
    }

    public void axpy(int N, double alpha, double[] X, double[] Y) {
        Daxpy.daxpy(N, alpha, X, 0, 1, Y, 0, 1);
    }

    public void scal(int N, double alpha, double[] X) {
        Dscal.dscal(N, alpha, X, 0, 1);
    }

    public void gemv(Transpose TransA, int M, int N, double alpha, double[] A,
            int lda, double[] X, double beta, double[] Y) {
        Dgemv.dgemv(trans(TransA), M, N, alpha, A, 0, lda, X, 0, 1, beta, Y, 0,
                1);
    }

    public void gbmv(Transpose TransA, int M, int N, int KL, int KU,
            double alpha, double[] A, int lda, double[] X, double beta,
            double[] Y) {
        Dgbmv.dgbmv(trans(TransA), M, N, KL, KU, alpha, A, 0, lda, X, 0, 1,
                beta, Y, 0, 1);
    }

    public void trmv(UpLo uplo, Transpose TransA, Diag diag, int N, double[] A,
            int lda, double[] X) {
        Dtrmv.dtrmv(uplo(uplo), trans(TransA), diag(diag), N, A, 0, lda, X, 0,
                1);
    }

    public void tbmv(UpLo uplo, Transpose TransA, Diag diag, int N, int K,
            double[] A, int lda, double[] X) {
        Dtbmv.dtbmv(uplo(uplo), trans(TransA), diag(diag), N, K, A, 0, lda, X,
                0, 1);
    }

    public void tpmv(UpLo uplo, Transpose TransA, Diag diag, int N,
            double[] Ap, double[] X) {
        Dtpmv.dtpmv(uplo(uplo), trans(TransA), diag(diag), N, Ap, 0, X, 0, 1);
    }

    public void trsv(UpLo uplo, Transpose TransA, Diag diag, int N, double[] A,
            int lda, double[] X) {
        Dtrsv.dtrsv(uplo(uplo), trans(TransA), diag(diag), N, A, 0, lda, X, 0,
                1);
    }

    public void tbsv(UpLo uplo, Transpose TransA, Diag diag, int N, int K,
            double[] A, int lda, double[] X) {
        Dtbsv.dtbsv(uplo(uplo), trans(TransA), diag(diag), N, K, A, 0, lda, X,
                0, 1);
    }

    public void tpsv(UpLo uplo, Transpose TransA, Diag diag, int N,
            double[] Ap, double[] X) {
        Dtpsv.dtpsv(uplo(uplo), trans(TransA), diag(diag), N, Ap, 0, X, 0, 1);
    }

    public void symv(UpLo uplo, int N, double alpha, double[] A, int lda,
            double[] X, double beta, double[] Y) {
        Dsymv.dsymv(uplo(uplo), N, alpha, A, 0, lda, X, 0, 1, beta, Y, 0, 1);
    }

    public void sbmv(UpLo uplo, int N, int K, double alpha, double[] A,
            int lda, double[] X, double beta, double[] Y) {
        Dsbmv.dsbmv(uplo(uplo), N, K, alpha, A, 0, lda, X, 0, 1, beta, Y, 0, 1);
    }

    public void spmv(UpLo uplo, int N, double alpha, double[] Ap, double[] X,
            double beta, double[] Y) {
        Dspmv.dspmv(uplo(uplo), N, alpha, Ap, 0, X, 0, 1, beta, Y, 0, 1);
    }

    public void ger(int M, int N, double alpha, double[] X, double[] Y,
            double[] A, int lda) {
        Dger.dger(M, N, alpha, X, 0, 1, Y, 0, 1, A, 0, lda);
    }

    public void syr(UpLo uplo, int N, double alpha, double[] X, double[] A,
            int lda) {
        Dsyr.dsyr(uplo(uplo), N, alpha, X, 0, 1, A, 0, lda);
    }

    public void spr(UpLo uplo, int N, double alpha, double[] X, double[] Ap) {
        Dspr.dspr(uplo(uplo), N, alpha, X, 0, 1, Ap, 0);
    }

    public void syr2(UpLo uplo, int N, double alpha, double[] X, double[] Y,
            double[] A, int lda) {
        Dsyr2.dsyr2(uplo(uplo), N, alpha, X, 0, 1, Y, 0, 1, A, 0, lda);
    }

    public void spr2(UpLo uplo, int N, double alpha, double[] X, double[] Y,
            double[] A) {
        Dspr2.dspr2(uplo(uplo), N, alpha, X, 0, 1, Y, 0, 1, A, 0);
    }
   */

  def gemm(transA : Boolean, transB : Boolean, M : Int, N : Int, K : Int,
           alpha : Double, A : Array[Double], lda : Int, B : Array[Double],
           ldb : Int, beta : Double, C : Array[Double], ldc : Int) =
    Dgemm.dgemm(trans(transA), trans(transB), M, N, K, alpha, A, 0, lda, B,
                0, ldb, beta, C, 0, ldc);

    /*
    public void symm(Side side, UpLo uplo, int M, int N, double alpha,
            double[] A, int lda, double[] B, int ldb, double beta, double[] C,
            int ldc) {
        Dsymm.dsymm(side(side), uplo(uplo), M, N, alpha, A, 0, lda, B, 0, ldb,
                beta, C, 0, ldc);
    }

    public void syrk(UpLo uplo, Transpose Trans, int N, int K, double alpha,
            double[] A, int lda, double beta, double[] C, int ldc) {
        Dsyrk.dsyrk(uplo(uplo), trans(Trans), N, K, alpha, A, 0, lda, beta, C,
                0, ldc);
    }

    public void syr2k(UpLo uplo, Transpose Trans, int N, int K, double alpha,
            double[] A, int lda, double[] B, int ldb, double beta, double[] C,
            int ldc) {
        Dsyr2k.dsyr2k(uplo(uplo), trans(Trans), N, K, alpha, A, 0, lda, B, 0,
                ldb, beta, C, 0, ldc);
    }

    public void trmm(Side side, UpLo uplo, Transpose TransA, Diag diag, int M,
            int N, double alpha, double[] A, int lda, double[] B, int ldb) {
        Dtrmm.dtrmm(side(side), uplo(uplo), trans(TransA), diag(diag), M, N,
                alpha, A, 0, lda, B, 0, ldb);
    }

    public void trsm(Side side, UpLo uplo, Transpose TransA, Diag diag, int M,
            int N, double alpha, double[] A, int lda, double[] B, int ldb) {
        Dtrsm.dtrsm(side(side), uplo(uplo), trans(TransA), diag(diag), M, N,
                alpha, A, 0, lda, B, 0, ldb);
    }
     */

  private def trans(trans : Boolean) =
    if (trans) "T" else "N";

  /*
    private String uplo(UpLo uplo) {
        if (uplo == UpLo.Lower)
            return "L";
        else
            return "U";
    }

    private String diag(Diag diag) {
        if (diag == Diag.NonUnit)
            return "N";
        else
            return "U";
    }

    private String side(Side side) {
        if (side == Side.Left)
            return "L";
        else
            return "R";
    }
    */
}