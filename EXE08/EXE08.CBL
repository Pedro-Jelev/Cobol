       IDENTIFICATION DIVISION.
       PROGRAM-ID.    EXE08.
       DATE-WRITTEN.  20/03/2024.
       DATE-COMPILED.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
           SOURCE-COMPUTER. IBM-PC.
           OBJECT-COMPUTER. IBM-PC.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CADFUN   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
           
           SELECT CAD-SAI   ASSIGN TO DISK
           ORGANIZATION IS  LINE  SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CADFUN
           LABEL  RECORD  ARE STANDARD
           VALUE  OF FILE-ID IS "CADFUN.DAT".
       
       01 REG-ENT.
           03 CODIGO-ENT     PIC 9(05).
           03 NOME-ENT       PIC X(20).
           03 SALARIO-ENT    PIC 9(05)V99.
       
       FD CAD-SAI
           LABEL RECORD ARE STANDARD
           VALUE OF FILE-ID IS "CAD-SAI.DAT".

       01 REG-SAI.
           03 CODIGO-SAI     PIC 9(04).
           03 NOME-SAI       PIC X(20).
           03 SALARIO-SAI    PIC 9(05)V99.


       WORKING-STORAGE SECTION.
       77 FIM-ARQ     PIC X(03) VALUE "NAO".
       77 REAJUSTE   PIC 9(05)V99 VALUE ZEROS.
       
       PROCEDURE DIVISION.
       PGM-EM01EX08.
           PERFORM INICIO.
           PERFORM PRINCIPAL
                  UNTIL FIM-ARQ EQUAL "SIM".

           PERFORM TERMINO.
           STOP RUN.

       INICIO.
           OPEN INPUT CADFUN
                OUTPUT CAD-SAI.
           PERFORM LEITURA.

       LEITURA.
           READ CADFUN
                AT END
                MOVE "SIM" TO FIM-ARQ.

       PRINCIPAL.
           PERFORM CALCREAJUSTE.
           PERFORM GRAVACAO.
           PERFORM LEITURA.
       
       CALCREAJUSTE.
           IF (SALARIO-ENT > 2000) THEN
               MULTIPLY SALARIO-ENT
                   BY 0.1
                   GIVING REAJUSTE
           ELSE
             IF ((SALARIO-ENT > 1000) AND (SALARIO-ENT NOT GREATER THAN 2000)) THEN
               MULTIPLY SALARIO-ENT
                   BY 0.11
                   GIVING REAJUSTE
             ELSE
               MULTIPLY SALARIO-ENT
                   BY 0.12
                   GIVING REAJUSTE.
           ADD
               SALARIO-ENT
               TO REAJUSTE.

       GRAVACAO.
           MOVE CODIGO-ENT TO CODIGO-SAI.
           MOVE NOME-ENT TO NOME-SAI.
           MOVE REAJUSTE TO SALARIO-SAI.
           WRITE REG-SAI.

       TERMINO.
           CLOSE  CADFUN
                  CAD-SAI.
