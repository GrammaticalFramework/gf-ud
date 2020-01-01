concrete BackupsEng of Backups = CatEng **

open SyntaxEng, MarkupEng in

{

lincat
  Backup = Adv ;
  ListBackup = Mark ;

lin
  AddBackupS b s = MarkupS (backupMark b) s ;
  AddBackupCN b s = MarkupCN (backupMark b) s ;
  AddBackupNP b s = MarkupNP (backupMark b) s ;
  AddBackupAP b s = MarkupAP (backupMark b) s ;
  AddBackupAdv b s = MarkupAdv (backupMark b) s ;
  AddBackupUtt b s = MarkupUtt (backupMark b) s ;
  AddBackupText b s = MarkupText (backupMark b) s ;

  AdvBackup adv = adv ;
  APBackup adv = uttBackup (mkUtt adv) ;
  NPBackup adv = uttBackup (mkUtt adv) ;
  CNBackup adv = uttBackup (mkUtt adv) ;

  BaseBackup = lin Mark {begin = "[" ; end = "]"} ;  
  ConsBackup b bs = lin Mark {begin = bs.begin ; end = b.s ++ bs.end} ;

----
  CopBackup adv = adv ;
  be__x__Cop = lin Adv {s = "BE"} ;


oper
  uttBackup : {s : Str} -> Adv = \u -> lin Adv u ;

  backupMark : Mark -> Mark = \m -> lin Mark {begin = [] ; end = m.begin ++ m.end} ; 
}