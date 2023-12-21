      *Mainframe DevOps Demo Program
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CBSBSDG.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
        01 WS-ACCOUNT-NO-T PIC S9(18).
        01 WS-ACCOUNT-STATUS  PIC X(10).
        01 WK-CONSTANTS.
           05 WK-INACTIVE              PIC X(10) VALUE 'INACTIVE'.
      *
        01 WG-WORK-AREA.
           02 CSDGREQ.
               COPY CSDGREQ.
           02 CSDGRES.
               COPY CSDGRES.
      *
           EXEC SQL
                INCLUDE CBSMST
           END-EXEC.
      *
           EXEC SQL
                INCLUDE SQLCA
           END-EXEC.
      *
       LINKAGE SECTION.
      *
       PROCEDURE DIVISION.
           MOVE LOW-VALUES             TO DCLCBS-ACCT-MSTR-DTL.
      *    MOVE ACCOUNT-NO             TO WS-ACCOUNT-NO-T.
           MOVE 100000001001           TO WS-ACCOUNT-NO-T.
           MOVE SPACE                  TO CUSTOMER-NAME.
           MOVE SPACE                  TO SYS-DATE.
           MOVE SPACE                  TO SYS-TIME.
           MOVE ZEROS                  TO CUSTOMER-ID.
      *
           PERFORM ACCT-VALID
              THRU ACCT-VALID-EXIT.
      *
           GOBACK.

        ACCT-VALID.
      *
           MOVE     WS-ACCOUNT-NO-T    TO H1-ACCOUNT-NUMBER
           DISPLAY "ACCT NO. FROM INPUT" H1-ACCOUNT-NUMBER
      *
           EXEC SQL
                SELECT CURRENT TIME
                INTO   :H1-ACCOUNT-STATUS
                FROM   SYSIBM.SYSDUMMY1
           END-EXEC
      *
           MOVE H1-ACCOUNT-STATUS      TO SYS-TIME
           DISPLAY 'TIME'              SYS-TIME
      *
           EXEC SQL
                SELECT CURRENT DATE
                INTO :H1-ACCOUNT-STATUS
                FROM   SYSIBM.SYSDUMMY1
           END-EXEC
      *
           MOVE H1-ACCOUNT-STATUS       TO SYS-DATE
           DISPLAY 'DATE'               SYS-DATE
      *
           EXEC SQL
                SELECT   *
                INTO :DCLCBS-ACCT-MSTR-DTL
                FROM CBS_ACCT_MSTR_DTL
                WHERE ACCOUNT_NUMBER=:H1-ACCOUNT-NUMBER
           END-EXEC.
      *
           DISPLAY "SQLCODE:" SQLCODE
      *
           EVALUATE SQLCODE
            WHEN 0
             DISPLAY H1-ACCOUNT-NUMBER
             DISPLAY H1-UPD-USERID
             DISPLAY H1-ACCOUNT-STATUS
             DISPLAY H1-CUSTOMER-ID
             DISPLAY H1-PRODUCT-CODE
             DISPLAY 'ACCOUNT IS AVAILABLE'
             MOVE "SUCCESSFUL" TO MESSAGES
             MOVE H1-ACCOUNT-NAME TO CUSTOMER-NAME
             COMPUTE CUSTOMER-ID = H1-CUSTOMER-ID
             PERFORM    ACCT-STATUS
             THRU       ACCT-STATUS-EXIT
             DISPLAY 'MESSAGES:'
            WHEN 100
             MOVE "ACCOUNT DOES NOT EXIT WITH BANK" TO MESSAGES
             DISPLAY "MESSAGES:" MESSAGES
            WHEN OTHER
             DISPLAY "SQLCODE1:" SQLCODE
             MOVE "SQL ERROR" TO MESSAGES
             DISPLAY "MESSAGES:" MESSAGES
           END-EVALUATE.
        ACCT-VALID-EXIT.
           EXIT.
        ACCT-STATUS.
           EXEC SQL
               SELECT
                    ACCOUNT_STATUS
               INTO
                    :H1-ACCOUNT-STATUS
               FROM CBS_ACCT_MSTR_DTL
               WHERE ACCOUNT_NUMBER=:H1-ACCOUNT-NUMBER
           END-EXEC.
      *
           EVALUATE SQLCODE
            WHEN 0
             DISPLAY H1-ACCOUNT-STATUS(1:6)
             MOVE H1-ACCOUNT-STATUS TO WS-ACCOUNT-STATUS
             DISPLAY WS-ACCOUNT-STATUS
             DISPLAY 'ACCOUNT STATUS IS FETCHED'
             MOVE "SUCCESSFUL"          TO MESSAGES
             DISPLAY "MESSAGES:" MESSAGES
      *
             PERFORM CHECK-ACCT-STATUS
                THRU CHECK-ACCT-STATUS-EXIT
      *
            WHEN 100
             MOVE "NO RECORD FOUND" TO MESSAGES
             DISPLAY "MESSAGES:" MESSAGES
            WHEN OTHER
             DISPLAY "SQLCODE2:" SQLCODE
             MOVE "SQL ERROR" TO MESSAGES
             DISPLAY "MESSAGES:" MESSAGES
           END-EVALUATE.
        ACCT-STATUS-EXIT.
           EXIT.
      *
        CHECK-ACCT-STATUS.
           DISPLAY 'CHECK STATUS PARA'
           EVALUATE WS-ACCOUNT-STATUS
              WHEN 'ACTIVE    '
               DISPLAY 'DEREGISTER STARTING'
               MOVE 'ACCOUNT DEREGISTERING' TO MESSAGES
               PERFORM DEREG-ACCT-STATS
                  THRU DEREG-ACCT-STATS-EXIT
              WHEN 'INACTIVE'
               MOVE 'CUSTOMER IS NOT REGISTERED' TO MESSAGES
              WHEN 'OTHER'
               DISPLAY 'NOT Y OR N'
               MOVE 'PLEASE CONTACT BANK' TO MESSAGES
           END-EVALUATE.
        CHECK-ACCT-STATUS-EXIT.
            EXIT.
      *
        DEREG-ACCT-STATS.
           MOVE H1-ACCOUNT-NAME TO CUSTOMER-NAME.
           MOVE H1-CUSTOMER-ID  TO CUSTOMER-ID.
           DISPLAY 'DEREGISTER PARA'
           EXEC SQL
                UPDATE CBS_ACCT_MSTR_DTL
                   SET ACCOUNT_STATUS = :WK-INACTIVE
                 WHERE ACCOUNT_NUMBER = :H1-ACCOUNT-NUMBER
           END-EXEC.
           DISPLAY SQLCODE
            MOVE "CUSTOMER DEREGISTERED SUCESSFULLY" TO MESSAGES.
        DEREG-ACCT-STATS-EXIT.
            EXIT.