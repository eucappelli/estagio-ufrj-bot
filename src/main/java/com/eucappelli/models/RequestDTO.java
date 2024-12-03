package com.eucappelli.models;

import com.eucappelli.enums.RequestStatus;
import com.eucappelli.utils.Strings;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class RequestDTO {
    private long id;
    private Long userId;
    private RequestStatus status;
    private Date date;
    private Date lastUpdate;
    private UserDTO user;
    private String reason;

    public String requestInfo() {
        DateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm");
        return String.format("%s: %s\n%s: %s\n%s: %s\n%s: %s", Strings.STATUS, getStatusString(), Strings.SENT_DATE, dateFormat.format(date),
                Strings.LAST_UPDATE, lastUpdate == null ? "-" : dateFormat.format(lastUpdate), Strings.REASON, reason == null ? "-" : reason);
    }

    private String getStatusString() {
        return status == RequestStatus.AUTHORIZED ? Strings.AUTHORIZED :
                status == RequestStatus.DENIED ? Strings.DENIED :
                        status == RequestStatus.SENT ? Strings.SENT :
                                status == RequestStatus.AUTHORIZED_UNDER_CONDITION ? Strings.AUTHORIZED_UNDER_CONDITION : Strings.NONE;
    }
}
