       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXE02.
       DATE-WRITTEN.  20/03/2024.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADALU   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
           
           SELECT CADATU   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CADALU
           LABEL  RECORD  ARE STANDARD
           VALUE  OF FILE-ID IS "CADALU.DAT".
       
       01 REG-ENT.

           03 NUMERO-ENT       PIC 9(05).
           03 NOME-ENT         PIC X(20).
           03 SEXO-ENT         PIC X(01).
           03 DATA-NASCIMENTO-ENT.
              04 DD-ENT        PIC 9(02).
              04 MM-ENT        PIC 9(02).
              04 AAAA-ENT      PIC 9(04).
       
       FD CADATU
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CADATU.DAT".

       01 REG-SAI.
           03 NUMERO-SAI       PIC 9(05).
           03 NOME-SAI         PIC X(20).
           03 DATA-NASCIMENTO-SAI.
              04 DD-SAI        PIC 9(02).
              04 MM-SAI        PIC 9(02).
              04 AAAA-SAI      PIC 9(04). 

       WORKING-STORAGE SECTION.
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       
       PROCEDURE DIVISION.
       PGM-EM01EX02.
           PERFORM INICIO.

           PERFORM PRINCIPAL
                  UNTIL FIM-ARQ EQUAL "SIM".

           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADALU
                OUTPUT CADATU.
           PERFORM LEITURA.

       LEITURA.
           READ CADALU
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM GRAVACAO.
           PERFORM LEITURA.

       GRAVACAO.
           MOVE NUMERO-ENT TO NUMERO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE DD-ENT TO DD-SAI.
           MOVE MM-ENT TO MM-SAI.
           MOVE AAAA-ENT TO AAAA-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE  CADALU
                  CADATU.
