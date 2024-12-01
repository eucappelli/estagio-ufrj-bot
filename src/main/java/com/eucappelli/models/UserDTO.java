package com.eucappelli.models;

import com.eucappelli.utils.Strings;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.NotBlank;

@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
public class UserDTO {

    @NotBlank
    private Long id;
    private String fullName;
    private String dre;
    private Boolean isRenewal;
    private Integer renewalNumber;
    private Integer requestNumber;
    private Integer requestId;
    private String cra;
    private Boolean hasCompleted;
    private Integer leftToComplete;
    private FileDTO boaFile;
    private FileDTO boletimFile;
    private ContractDTO contract;

    public String getFirstName() {
        return fullName.split("\\s")[0];
    }

    public String getRenewal() {
        return isRenewal ? Strings.YES : Strings.NO;
    }

    public String getCompleted() {
        return hasCompleted ? Strings.YES : Strings.NO;
    }

    public String infoConfirmation() {
        return String.format(Strings.INFO_CONFIRMATION,
                fullName, dre, isRenewal ? "renovação" : "estágio", isRenewal ? renewalNumber : requestNumber, cra, leftToComplete);
    }
}
