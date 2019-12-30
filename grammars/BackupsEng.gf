concrete BackupsEng of Backups = CatEng **

open SyntaxEng, MarkupEng in

{

lincat
  Backup = Mark ;
  ListBackup = Mark ;

lin
  AddBackupS b s = MarkupS b s ;
  AddBackupCN b s = MarkupCN b s ;
  AddBackupNP b s = MarkupNP b s ;
  AddBackupAP b s = MarkupAP b s ;
  AddBackupAdv b s = MarkupAdv b s ;
  AddBackupUtt b s = MarkupUtt b s ;

  AdvBackup adv = uttMark (mkUtt adv) ;
  APBackup adv = uttMark (mkUtt adv) ;
  NPBackup adv = uttMark (mkUtt adv) ;
  CNBackup adv = uttMark (mkUtt adv) ;

  BaseBackup b = lin Mark {begin = "[" ; end = b.end ++ "]"} ;  -- just add to the end part after [
  ConsBackup b bs = lin Mark {begin = bs.begin ; end = b.end ++ bs.end} ;

oper
  uttMark : Utt -> Mark = \u -> lin Mark {begin = "" ; end = u.s} ;
}