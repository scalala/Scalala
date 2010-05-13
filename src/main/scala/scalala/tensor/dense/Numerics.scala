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
package scalala;
package tensor;
package dense;

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

    sealed trait JobSVD { def repr: String }
    object JobSVD {
      case object All extends JobSVD { def repr = "A" };
      case object Some extends JobSVD { def repr = "S" };
      case object Overwrite extends JobSVD { def repr = "O" };
      case object None extends JobSVD { def repr = "N" };
    }

     def gesvd(jobu: JobSVD, jobvt: JobSVD, m: Int, n: Int, A: Array[Double],
            s: Array[Double], U: Array[Double], Vt: Array[Double], work: Array[Double], lwork: Int):Int = {
        val info = new intW(0);
        Dgesvd.dgesvd(jobu.repr, jobvt.repr, m, n, A, 0, ld(m), s, 0, U,
                0, ld(m), Vt, 0, ld(n), work, 0, lwork, info);
        info.`val`;
    }

    def gesdd(jobz: JobSVD, m: Int, n: Int, A: Array[Double],
            s: Array[Double], U: Array[Double], Vt: Array[Double], work: Array[Double], lwork: Int, iwork: Array[Int]):Int = {
        val info = new intW(0);
        Dgesdd.dgesdd(jobz.repr, m, n, A, 0, ld(m), s, 0, U,
                0, ld(m), Vt, 0, ld(n), work, 0, lwork, iwork, 0, info);
        info.`val`;
    }


  /*
    public gelss: Int(m: Int, n: Int, nrhs: Int, A: Array[Double], B: Array[Double],
            s: Array[Double], double rcond, int[] rank, work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        intW rankW = new intW(rank[0]);
        Dgelss.dgelss(m, n, nrhs, A, 0, ld(m), B, 0, ld(m, n), s, 0, rcond,
                rankW, work, 0, lwork, info);
        rank[0] = rankW.val;
        return info.val;
    }

    public gelsd: Int(m: Int, n: Int, nrhs: Int, A: Array[Double], B: Array[Double],
            s: Array[Double], double rcond, int[] rank, work: Array[Double], lwork: Int,
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

    def geev(jobvl: Boolean, jobvr: Boolean, n: Int, A: Array[Double],
          wr: Array[Double], wi: Array[Double], Vl: Array[Double], Vr: Array[Double],
          work: Array[Double], lwork: Int) = {
        val info = new intW(0);
        Dgeev.dgeev(jobEig(jobvl), jobEig(jobvr), n, A, 0, ld(n), wr, 0, wi, 0,
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

    public spev: Int(JobEig jobz, UpLo uplo, n: Int, Ap: Array[Double], w: Array[Double],
            Z: Array[Double], work: Array[Double]) {
        intW info = new intW(0);
        Dspev.dspev(jobEig(jobz), uplo(uplo), n, Ap, 0, w, 0, Z, 0, ld(n),
                work, 0, info);
        return info.val;
    }

    public sbev: Int(JobEig jobz, UpLo uplo, n: Int, kd: Int, Ab: Array[Double],
            w: Array[Double], Z: Array[Double], work: Array[Double]) {
        intW info = new intW(0);
        Dsbev.dsbev(jobEig(jobz), uplo(uplo), n, kd, Ab, 0, ld(kd + 1), w, 0,
                Z, 0, ld(n), work, 0, info);
        return info.val;
    }

    public stev: Int(JobEig jobz, n: Int, d: Array[Double], e: Array[Double], Z: Array[Double],
            work: Array[Double]) {
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
    public gbsv: Int(n: Int, kl: Int, ku: Int, nrhs: Int, Ab: Array[Double], int[] ipiv,
            B: Array[Double]) {
        intW info = new intW(0);
        Dgbsv.dgbsv(n, kl, ku, nrhs, Ab, 0, ld(2 * kl + ku + 1), ipiv, 0, B, 0,
                ld(n), info);
        return info.val;
    }

    public gtsv: Int(n: Int, nrhs: Int, dl: Array[Double], d: Array[Double], du: Array[Double],
            B: Array[Double]) {
        intW info = new intW(0);
        Dgtsv.dgtsv(n, nrhs, dl, 0, d, 0, du, 0, B, 0, ld(n), info);
        return info.val;
    }

    public posv: Int(UpLo uplo, n: Int, nrhs: Int, A: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dposv.dposv(uplo(uplo), n, nrhs, A, 0, ld(n), B, 0, ld(n), info);
        return info.val;
    }

    public ppsv: Int(UpLo uplo, n: Int, nrhs: Int, Ap: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dppsv.dppsv(uplo(uplo), n, nrhs, Ap, 0, B, 0, ld(n), info);
        return info.val;
    }

    public pptrf: Int(UpLo uplo, n: Int, Ap: Array[Double]) {
        intW info = new intW(0);
        Dpptrf.dpptrf(uplo(uplo), n, Ap, 0, info);
        return info.val;
    }

    public pptrs: Int(UpLo uplo, n: Int, nrhs: Int, Ap: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dpptrs.dpptrs(uplo(uplo), n, nrhs, Ap, 0, B, 0, ld(n), info);
        return info.val;
    }

    public ppcon: Int(UpLo uplo, n: Int, Ap: Array[Double], double anorm,
            rcond: Array[Double], work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dppcon.dppcon(uplo(uplo), n, Ap, 0, anorm, rcondW, work, 0, iwork, 0,
                info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public pbsv: Int(UpLo uplo, n: Int, kd: Int, nrhs: Int, Ab: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dpbsv.dpbsv(uplo(uplo), n, kd, nrhs, Ab, 0, ld(kd + 1), B, 0, ld(n),
                info);
        return info.val;
    }

    public pbtrf: Int(UpLo uplo, n: Int, kd: Int, Ab: Array[Double]) {
        intW info = new intW(0);
        Dpbtrf.dpbtrf(uplo(uplo), n, kd, Ab, 0, ld(kd + 1), info);
        return info.val;
    }

    public pbtrs: Int(UpLo uplo, n: Int, kd: Int, nrhs: Int, Ab: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dpbtrs.dpbtrs(uplo(uplo), n, kd, nrhs, Ab, 0, ld(kd + 1), B, 0, ld(n),
                info);
        return info.val;
    }

    public pbcon: Int(UpLo uplo, n: Int, kd: Int, Ab: Array[Double], double anorm,
            rcond: Array[Double], work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dpbcon.dpbcon(uplo(uplo), n, kd, Ab, 0, ld(kd + 1), anorm, rcondW,
                work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public ptsv: Int(n: Int, nrhs: Int, d: Array[Double], e: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dptsv.dptsv(n, nrhs, d, 0, e, 0, B, 0, ld(n), info);
        return info.val;
    }

    public sysv: Int(UpLo uplo, n: Int, nrhs: Int, A: Array[Double], int[] ipiv,
            B: Array[Double], work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dsysv.dsysv(uplo(uplo), n, nrhs, A, 0, ld(n), ipiv, 0, B, 0, ld(n),
                work, 0, lwork, info);
        return info.val;
    }

    public spsv: Int(UpLo uplo, n: Int, nrhs: Int, Ap: Array[Double], int[] ipiv,
            B: Array[Double]) {
        intW info = new intW(0);
        Dspsv.dspsv(uplo(uplo), n, nrhs, Ap, 0, ipiv, 0, B, 0, ld(n), info);
        return info.val;
    }

    public geqrf: Int(m: Int, n: Int, A: Array[Double], tau: Array[Double], work: Array[Double],
            lwork: Int) {
        intW info = new intW(0);
        Dgeqrf.dgeqrf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public ormqr: Int(Side side, Transpose trans, m: Int, n: Int, k: Int,
            A: Array[Double], tau: Array[Double], C: Array[Double], work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dormqr.dormqr(side(side), trans(trans), m, n, k, A, 0,
                side == Side.Left ? ld(m) : ld(n), tau, 0, C, 0, ld(m), work,
                0, lwork, info);
        return info.val;
    }

    public orgqr: Int(m: Int, n: Int, k: Int, A: Array[Double], tau: Array[Double],
            work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dorgqr.dorgqr(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public geqlf: Int(m: Int, n: Int, A: Array[Double], tau: Array[Double], work: Array[Double],
            lwork: Int) {
        intW info = new intW(0);
        Dgeqlf.dgeqlf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public ormql: Int(Side side, Transpose trans, m: Int, n: Int, k: Int,
            A: Array[Double], tau: Array[Double], C: Array[Double], work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dormql.dormql(side(side), trans(trans), m, n, k, A, 0,
                side == Side.Left ? ld(m) : ld(n), tau, 0, C, 0, ld(m), work,
                0, lwork, info);
        return info.val;
    }

    public orgql: Int(m: Int, n: Int, k: Int, A: Array[Double], tau: Array[Double],
            work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dorgql.dorgql(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public gerqf: Int(m: Int, n: Int, A: Array[Double], tau: Array[Double], work: Array[Double],
            lwork: Int) {
        intW info = new intW(0);
        Dgerqf.dgerqf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public ormrq: Int(Side side, Transpose trans, m: Int, n: Int, k: Int,
            A: Array[Double], tau: Array[Double], C: Array[Double], work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dormrq.dormrq(side(side), trans(trans), m, n, k, A, 0, ld(k), tau, 0,
                C, 0, ld(m), work, 0, lwork, info);
        return info.val;
    }

    public orgrq: Int(m: Int, n: Int, k: Int, A: Array[Double], tau: Array[Double],
            work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dorgrq.dorgrq(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public gelqf: Int(m: Int, n: Int, A: Array[Double], tau: Array[Double], work: Array[Double],
            lwork: Int) {
        intW info = new intW(0);
        Dgelqf.dgelqf(m, n, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public ormlq: Int(Side side, Transpose trans, m: Int, n: Int, k: Int,
            A: Array[Double], tau: Array[Double], C: Array[Double], work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dormlq.dormlq(side(side), trans(trans), m, n, k, A, 0, ld(k), tau, 0,
                C, 0, ld(m), work, 0, lwork, info);
        return info.val;
    }

    public orglq: Int(m: Int, n: Int, k: Int, A: Array[Double], tau: Array[Double],
            work: Array[Double], lwork: Int) {
        intW info = new intW(0);
        Dorglq.dorglq(m, n, k, A, 0, ld(m), tau, 0, work, 0, lwork, info);
        return info.val;
    }

    public gbtrf: Int(m: Int, n: Int, kl: Int, ku: Int, Ab: Array[Double], int[] ipiv) {
        intW info = new intW(0);
        Dgbtrf.dgbtrf(m, n, kl, ku, Ab, 0, 2 * kl + ku + 1, ipiv, 0, info);
        return info.val;
    }

    public gbtrs: Int(Transpose trans, n: Int, kl: Int, ku: Int, nrhs: Int,
            Ab: Array[Double], int[] ipiv, B: Array[Double]) {
        intW info = new intW(0);
        Dgbtrs.dgbtrs(trans(trans), n, kl, ku, nrhs, Ab, 0, 2 * kl + ku + 1,
                ipiv, 0, B, 0, ld(n), info);
        return info.val;
    }

    public getrf: Int(m: Int, n: Int, A: Array[Double], int[] ipiv) {
        intW info = new intW(0);
        Dgetrf.dgetrf(m, n, A, 0, ld(m), ipiv, 0, info);
        return info.val;
    }

    public getrs: Int(Transpose trans, n: Int, nrhs: Int, A: Array[Double], int[] ipiv,
            B: Array[Double]) {
        intW info = new intW(0);
        Dgetrs.dgetrs(trans(trans), n, nrhs, A, 0, ld(n), ipiv, 0, B, 0, ld(n),
                info);
        return info.val;
    }

    public gecon: Int(Norm norm, n: Int, A: Array[Double], double anorm,
            rcond: Array[Double], work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgecon.dgecon(norm(norm), n, A, 0, ld(n), anorm, rcondW, work, 0,
                iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public gbcon: Int(Norm norm, n: Int, kl: Int, ku: Int, Ab: Array[Double], int[] ipiv,
            double anorm, rcond: Array[Double], work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgbcon.dgbcon(norm(norm), n, kl, ku, Ab, 0, ld(2 * kl + ku + 1), ipiv,
                0, anorm, rcondW, work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public gttrf: Int(n: Int, dl: Array[Double], d: Array[Double], du: Array[Double], du2: Array[Double],
            int[] ipiv) {
        intW info = new intW(0);
        Dgttrf.dgttrf(n, dl, 0, d, 0, du, 0, du2, 0, ipiv, 0, info);
        return info.val;
    }

    public gttrs: Int(Transpose trans, n: Int, nrhs: Int, dl: Array[Double], d: Array[Double],
            du: Array[Double], du2: Array[Double], int[] ipiv, B: Array[Double]) {
        intW info = new intW(0);
        Dgttrs.dgttrs(trans(trans), n, nrhs, dl, 0, d, 0, du, 0, du2, 0, ipiv,
                0, B, 0, ld(n), info);
        return info.val;
    }

    public gtcon: Int(Norm norm, n: Int, dl: Array[Double], d: Array[Double], du: Array[Double],
            du2: Array[Double], int[] ipiv, double anorm, rcond: Array[Double],
            work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dgtcon.dgtcon(norm(norm), n, dl, 0, d, 0, du, 0, du2, 0, ipiv, 0,
                anorm, rcondW, work, 0, iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }

    public trtrs: Int(UpLo uplo, Transpose trans, Diag diag, n: Int, nrhs: Int,
            A: Array[Double], lda: Int, B: Array[Double]) {
        intW info = new intW(0);
        Dtrtrs.dtrtrs(uplo(uplo), trans(trans), diag(diag), n, nrhs, A, 0, lda,
                B, 0, ld(n), info);
        return info.val;
    }

    public tptrs: Int(UpLo uplo, Transpose trans, Diag diag, n: Int, nrhs: Int,
            Ap: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dtptrs.dtptrs(uplo(uplo), trans(trans), diag(diag), n, nrhs, Ap, 0, B,
                0, ld(n), info);
        return info.val;
    }

    public tbtrs: Int(UpLo uplo, Transpose trans, Diag diag, n: Int, kd: Int,
            nrhs: Int, Ab: Array[Double], B: Array[Double]) {
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

    public potrs: Int(UpLo uplo, n: Int, nrhs: Int, A: Array[Double], B: Array[Double]) {
        intW info = new intW(0);
        Dpotrs.dpotrs(uplo(uplo), n, nrhs, A, 0, ld(n), B, 0, ld(n), info);
        return info.val;
    }

    public pocon: Int(UpLo uplo, n: Int, A: Array[Double], double anorm,
            rcond: Array[Double], work: Array[Double], int[] iwork) {
        intW info = new intW(0);
        doubleW rcondW = new doubleW(rcond[0]);
        Dpocon.dpocon(uplo(uplo), n, A, 0, ld(n), anorm, rcondW, work, 0,
                iwork, 0, info);
        rcond[0] = rcondW.val;
        return info.val;
    }
    */

    private def ld(n : Int) =
      math.max(1,n);

    private def ld(m : Int, n : Int) =
        math.max(1, math.max(m, n));

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

    */

    private def jobEig(computeEigenvectors: Boolean) = {
        if (!computeEigenvectors) 
            "N"
        else
            "V"
    }

    /*

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

    public sbevd: Int(JobEig jobz, UpLo uplo, n: Int, kd: Int, Ab: Array[Double],
            w: Array[Double], Z: Array[Double], work: Array[Double], lwork: Int, int[] iwork,
            liwork: Int) {
        intW info = new intW(0);
        Dsbevd.dsbevd(jobEig(jobz), uplo(uplo), n, kd, Ab, 0, ld(kd + 1), w, 0,
                Z, 0, ld(n), work, 0, lwork, iwork, 0, liwork, info);
        return info.val;
    }

    public spevd: Int(JobEig jobz, UpLo uplo, n: Int, Ap: Array[Double], w: Array[Double],
            Z: Array[Double], work: Array[Double], lwork: Int, int[] iwork, liwork: Int) {
        intW info = new intW(0);
        Dspevd.dspevd(jobEig(jobz), uplo(uplo), n, Ap, 0, w, 0, Z, 0, ld(n),
                work, 0, lwork, iwork, 0, liwork, info);
        return info.val;
    }

    public stevr: Int(JobEig jobz, JobEigRange range, n: Int, d: Array[Double],
            e: Array[Double], double vl, double vu, il: Int, iu: Int, double abstol,
            int[] m, w: Array[Double], Z: Array[Double], int[] isuppz, work: Array[Double],
            lwork: Int, int[] iwork, liwork: Int) {
        intW info = new intW(0);
        intW mW = new intW(0);
        Dstevr.dstevr(jobEig(jobz), jobEigRange(range), n, d, 0, e, 0, vl, vu,
                il, iu, abstol, mW, w, 0, Z, 0, ld(n), isuppz, 0, work, 0,
                lwork, iwork, 0, liwork, info);
        m[0] = mW.val;
        return info.val;
    }

    public syevr: Int(JobEig jobz, JobEigRange range, UpLo uplo, n: Int,
            A: Array[Double], double vl, double vu, il: Int, iu: Int, double abstol,
            int[] m, w: Array[Double], Z: Array[Double], int[] isuppz, work: Array[Double],
            lwork: Int, int[] iwork, liwork: Int) {
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
    public double nrm2(N: Int, X: Array[Double]) {
        return Dnrm2.dnrm2(N, X, 0, 1);
    }

    public double asum(N: Int, X: Array[Double]) {
        return Dasum.dasum(N, X, 0, 1);
    }

    public idamax: Int(N: Int, X: Array[Double]) {
        return Idamax.idamax(N, X, 0, 1);
    }

    public void swap(N: Int, X: Array[Double], Y: Array[Double]) {
        Dswap.dswap(N, X, 0, 1, Y, 0, 1);
    }

    public void copy(N: Int, X: Array[Double], Y: Array[Double]) {
        Dcopy.dcopy(N, X, 0, 1, Y, 0, 1);
    }

    public void axpy(N: Int, double alpha, X: Array[Double], Y: Array[Double]) {
        Daxpy.daxpy(N, alpha, X, 0, 1, Y, 0, 1);
    }

    public void scal(N: Int, double alpha, X: Array[Double]) {
        Dscal.dscal(N, alpha, X, 0, 1);
    }

    public void gemv(Transpose TransA, M: Int, N: Int, double alpha, A: Array[Double],
            lda: Int, X: Array[Double], double beta, Y: Array[Double]) {
        Dgemv.dgemv(trans(TransA), M, N, alpha, A, 0, lda, X, 0, 1, beta, Y, 0,
                1);
    }

    public void gbmv(Transpose TransA, M: Int, N: Int, KL: Int, KU: Int,
            double alpha, A: Array[Double], lda: Int, X: Array[Double], double beta,
            Y: Array[Double]) {
        Dgbmv.dgbmv(trans(TransA), M, N, KL, KU, alpha, A, 0, lda, X, 0, 1,
                beta, Y, 0, 1);
    }

    public void trmv(UpLo uplo, Transpose TransA, Diag diag, N: Int, A: Array[Double],
            lda: Int, X: Array[Double]) {
        Dtrmv.dtrmv(uplo(uplo), trans(TransA), diag(diag), N, A, 0, lda, X, 0,
                1);
    }

    public void tbmv(UpLo uplo, Transpose TransA, Diag diag, N: Int, K: Int,
            A: Array[Double], lda: Int, X: Array[Double]) {
        Dtbmv.dtbmv(uplo(uplo), trans(TransA), diag(diag), N, K, A, 0, lda, X,
                0, 1);
    }

    public void tpmv(UpLo uplo, Transpose TransA, Diag diag, N: Int,
            Ap: Array[Double], X: Array[Double]) {
        Dtpmv.dtpmv(uplo(uplo), trans(TransA), diag(diag), N, Ap, 0, X, 0, 1);
    }

    public void trsv(UpLo uplo, Transpose TransA, Diag diag, N: Int, A: Array[Double],
            lda: Int, X: Array[Double]) {
        Dtrsv.dtrsv(uplo(uplo), trans(TransA), diag(diag), N, A, 0, lda, X, 0,
                1);
    }

    public void tbsv(UpLo uplo, Transpose TransA, Diag diag, N: Int, K: Int,
            A: Array[Double], lda: Int, X: Array[Double]) {
        Dtbsv.dtbsv(uplo(uplo), trans(TransA), diag(diag), N, K, A, 0, lda, X,
                0, 1);
    }

    public void tpsv(UpLo uplo, Transpose TransA, Diag diag, N: Int,
            Ap: Array[Double], X: Array[Double]) {
        Dtpsv.dtpsv(uplo(uplo), trans(TransA), diag(diag), N, Ap, 0, X, 0, 1);
    }

    public void symv(UpLo uplo, N: Int, double alpha, A: Array[Double], lda: Int,
            X: Array[Double], double beta, Y: Array[Double]) {
        Dsymv.dsymv(uplo(uplo), N, alpha, A, 0, lda, X, 0, 1, beta, Y, 0, 1);
    }

    public void sbmv(UpLo uplo, N: Int, K: Int, double alpha, A: Array[Double],
            lda: Int, X: Array[Double], double beta, Y: Array[Double]) {
        Dsbmv.dsbmv(uplo(uplo), N, K, alpha, A, 0, lda, X, 0, 1, beta, Y, 0, 1);
    }

    public void spmv(UpLo uplo, N: Int, double alpha, Ap: Array[Double], X: Array[Double],
            double beta, Y: Array[Double]) {
        Dspmv.dspmv(uplo(uplo), N, alpha, Ap, 0, X, 0, 1, beta, Y, 0, 1);
    }

    public void ger(M: Int, N: Int, double alpha, X: Array[Double], Y: Array[Double],
            A: Array[Double], lda: Int) {
        Dger.dger(M, N, alpha, X, 0, 1, Y, 0, 1, A, 0, lda);
    }

    public void syr(UpLo uplo, N: Int, double alpha, X: Array[Double], A: Array[Double],
            lda: Int) {
        Dsyr.dsyr(uplo(uplo), N, alpha, X, 0, 1, A, 0, lda);
    }

    public void spr(UpLo uplo, N: Int, double alpha, X: Array[Double], Ap: Array[Double]) {
        Dspr.dspr(uplo(uplo), N, alpha, X, 0, 1, Ap, 0);
    }

    public void syr2(UpLo uplo, N: Int, double alpha, X: Array[Double], Y: Array[Double],
            A: Array[Double], lda: Int) {
        Dsyr2.dsyr2(uplo(uplo), N, alpha, X, 0, 1, Y, 0, 1, A, 0, lda);
    }

    public void spr2(UpLo uplo, N: Int, double alpha, X: Array[Double], Y: Array[Double],
            A: Array[Double]) {
        Dspr2.dspr2(uplo(uplo), N, alpha, X, 0, 1, Y, 0, 1, A, 0);
    }
   */

  def gemm(transA : Boolean, transB : Boolean, M : Int, N : Int, K : Int,
           alpha : Double, A : Array[Double], lda : Int, B : Array[Double],
           ldb : Int, beta : Double, C : Array[Double], ldc : Int) =
    Dgemm.dgemm(trans(transA), trans(transB), M, N, K, alpha, A, 0, lda, B,
                0, ldb, beta, C, 0, ldc);

    /*
    public void symm(Side side, UpLo uplo, M: Int, N: Int, double alpha,
            A: Array[Double], lda: Int, B: Array[Double], ldb: Int, double beta, C: Array[Double],
            ldc: Int) {
        Dsymm.dsymm(side(side), uplo(uplo), M, N, alpha, A, 0, lda, B, 0, ldb,
                beta, C, 0, ldc);
    }

    public void syrk(UpLo uplo, Transpose Trans, N: Int, K: Int, double alpha,
            A: Array[Double], lda: Int, double beta, C: Array[Double], ldc: Int) {
        Dsyrk.dsyrk(uplo(uplo), trans(Trans), N, K, alpha, A, 0, lda, beta, C,
                0, ldc);
    }

    public void syr2k(UpLo uplo, Transpose Trans, N: Int, K: Int, double alpha,
            A: Array[Double], lda: Int, B: Array[Double], ldb: Int, double beta, C: Array[Double],
            ldc: Int) {
        Dsyr2k.dsyr2k(uplo(uplo), trans(Trans), N, K, alpha, A, 0, lda, B, 0,
                ldb, beta, C, 0, ldc);
    }

    public void trmm(Side side, UpLo uplo, Transpose TransA, Diag diag, M: Int,
            N: Int, double alpha, A: Array[Double], lda: Int, B: Array[Double], ldb: Int) {
        Dtrmm.dtrmm(side(side), uplo(uplo), trans(TransA), diag(diag), M, N,
                alpha, A, 0, lda, B, 0, ldb);
    }

    public void trsm(Side side, UpLo uplo, Transpose TransA, Diag diag, M: Int,
            N: Int, double alpha, A: Array[Double], lda: Int, B: Array[Double], ldb: Int) {
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
