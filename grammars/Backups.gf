abstract Backups = Cat ** {

cat
  Backup ;
  [Backup] ;

fun
  AddBackupS : [Backup] -> S -> S ;
  AddBackupCN : [Backup] -> CN -> CN ;
  AddBackupNP : [Backup] -> NP -> NP ;
  AddBackupAP : [Backup] -> AP -> AP ;
  AddBackupAdv : [Backup] -> Adv -> Adv ;
  AddBackupUtt : [Backup] -> Utt -> Utt ;
  AddBackupText : [Backup] -> Text -> Text ;

  AdvBackup : Adv -> Backup ;
  AdVBackup : AdV -> Backup ;
  AdABackup : AdA -> Backup ;
  APBackup  : AP -> Backup ;
  NPBackup  : NP -> Backup ;
  CNBackup  : CN -> Backup ;
  CompBackup : Comp -> Backup ;

---- some common cases
cat Dummy ;
fun
  CopBackup : Dummy -> Backup ;
  be__x__Cop : Dummy ;
  
  CommaBackup : Dummy -> Backup ;
  ',__x__Comma' : Dummy ;
  
  TheBackup : Dummy -> Backup ;
  the__x__The : Dummy ;

}