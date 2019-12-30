abstract Backups = Cat ** {

cat
  Backup ;
  [Backup] {1} ;

fun
  AddBackupS : Backup -> S -> S ;
  AddBackupCN : Backup -> CN -> CN ;
  AddBackupNP : Backup -> NP -> NP ;
  AddBackupAP : Backup -> AP -> AP ;
  AddBackupAdv : Backup -> Adv -> Adv ;
  AddBackupUtt : Backup -> Utt -> Utt ;
  AddBackupText : Backup -> Text -> Text ;

  AdvBackup : Adv -> Backup ;
  APBackup  : AP -> Backup ;
  NPBackup  : NP -> Backup ;
  CNBackup  : CN -> Backup ;

}