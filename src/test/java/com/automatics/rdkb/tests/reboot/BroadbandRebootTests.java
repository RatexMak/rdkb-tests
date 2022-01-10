/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.reboot;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadbandRebootTests extends AutomaticsTestBase{

    
    /**
     * Test to verify the behavior of SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) using eCM
     * IP Address. Device should not reboot if we set docsDevResetNow as 2 using SNMP command. If we set docsDevResetNow
     * as 1, then device should reboot and comes up within 5 minutes.
     * 
     * <ol>
     * <li>Step 1 : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.</li>
     * <li>Step 2 : Verify device is not rebooting after setting SNMP MIB
     * DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.</li>
     * <li>Step 3 : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.</li>
     * <li>Step 4 : Verify device is going for reboot after setting SNMP MIB
     * DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.</li>
     * <li>Step 5 : Verify device comes up after successful reboot.</li>
     * <li>Step 6 : Verify RDKB_REBOOT:docsDevResetNow log in Consolelog.txt.0.</li>
     * </ol>
     * 
     * @param settop
     *            Settop to be used
     * @author Prabhakaran
     * @refactor anandam
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-REBOOT-5001")
    public void verifyDeviceRebootUsingDocsDevResetNowSnmpCommand(Dut settop) {
	// Variable declaration starts
	boolean status = false;
	String testCaseId = "TC-RDKB-REBOOT-501";
	String stepNumber = "s1";
	String errorMessage = null;
	String snmpSetOutput = null;
	String response = null;
	// Variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-REBOOT-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the docsDevResetNow using DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3)");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	LOGGER.info(
		"2. Verify device is not rebooting after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	LOGGER.info("3. Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	LOGGER.info(
		"4. Verify device is going for reboot after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	LOGGER.info("5. Verify device comes up after successful reboot.");
	LOGGER.info("6. Verify RDKB_REBOOT:docsDevResetNow log in Consolelog.txt.0.");
	LOGGER.info("#######################################################################################");

	try {
	    stepNumber = "S1";
	    errorMessage = "Unable to set SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) value as 2.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    LOGGER.info("STEP 1: ACTION : Execute SNMP Set command for MIB: .1.3.6.1.2.1.69.1.1.3 and set value as 2.");
	    LOGGER.info("STEP 1: EXPECTED : SNMP Set command should execute successfully and return output as 2.");
	    LOGGER.info("**********************************************************************************");
	    snmpSetOutput = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, settop, BroadBandSnmpMib.ECM_RESET_MIB.getOid()+".0",
		    SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_TWO);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_VALUE_TWO, snmpSetOutput);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL: SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) is set to value 2.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S2";
	    errorMessage = "Device went for reboot even after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify device is not rebooting after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    LOGGER.info("STEP 2: ACTION : Check device accessibility after every 30 seconds continously for 5 min.");
	    LOGGER.info("STEP 2: EXPECTED : Device shouldn't go for reboot.");
	    LOGGER.info("**********************************************************************************");
	    status = !CommonMethods.isSTBRebooted(tapEnv, settop, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.CONSTANT_10);
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL: Device didn't go for reboot after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 2.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    /*
	     * Due to large buffer size, it takes more time for searching in buffer. So clearing buffer and start
	     * buffering the trace.
	     */
	    tapEnv.cleanupTraceBuffer(settop);

	    stepNumber = "S3";
	    errorMessage = "Unable to set SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) value as 1.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify Setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.");
	    LOGGER.info("STEP 3: ACTION : Execute SNMP Set command for MIB: .1.3.6.1.2.1.69.1.1.3 and set value as 1.");
	    LOGGER.info("STEP 3: EXPECTED : SNMP Set command should execute successfully and return output as 1.");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.executeCommandUsingSsh(settop, BroadBandCommandConstants.COMMAND_TO_COPY_TO_NVRAM_CONSOLELOG);
	    snmpSetOutput = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, settop, BroadBandSnmpMib.ECM_RESET_MIB.getOid()+".0",
		    SnmpDataType.INTEGER, BroadBandTestConstants.STRING_VALUE_ONE);
	    status = BroadBandCommonUtils.compareValues(BroadBandTestConstants.CONSTANT_TXT_COMPARISON,
		    BroadBandTestConstants.STRING_VALUE_ONE, snmpSetOutput);
	    boolean isSTBRebooted = CommonMethods.isSTBRebooted(tapEnv, settop,
		    BroadBandTestConstants.TEN_SECOND_IN_MILLIS, BroadBandTestConstants.CONSTANT_60);

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL: SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) is set to value 1.");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S4";
	    errorMessage = "Device did not go for reboot even after setting SNMP MIB DOCS-CABLE-DEVICE-MIB::docsDevResetNow(.1.3.6.1.2.1.69.1.1.3) to 1.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify device is going for reboot and comes up later.");
	    LOGGER.info(
		    "STEP 4: ACTION : Check device went for reboot after every 30 seconds continously for 5 min and and once go for reboot, wait for device to come up.");
	    LOGGER.info("STEP 4: EXPECTED : Device should go for reboot and should come up with all processes up.");
	    LOGGER.info("**********************************************************************************");
	    if (isSTBRebooted) {
		LOGGER.info("Device rebooted successfully.");
		errorMessage = "Device is not coming up after successful reboot.";
		status = CommonMethods.waitForEstbIpAcquisition(tapEnv, settop);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: Device went for reboot and came up with all the processes.");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);

	    stepNumber = "S5";
	    errorMessage = "Unable to verify last reboot reason.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify last reboot reason.");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the WebPa Get command for following param: Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason.");
	    LOGGER.info("STEP 5: EXPECTED : Last reboot reason should be 'snmp-reboot'.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.verifySnmpRebootReason(settop, tapEnv);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: Last reboot reason is verified successfully as 'snmp-reboot'.");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, false);

	    stepNumber = "S6";
	    errorMessage = "Log message is not found after Rebooting the device thorugh docsDevResetNow.0 mib Since this logging might or might not appear,"
		    + " marking the step as NA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify RDKB_REBOOT log message after setting docsDevResetNow.0 mib in Consolelog.txt.0.");
	    LOGGER.info("STEP 6: ACTION : SSH the device and look for required message in device logs.");
	    LOGGER.info(
		    "STEP 6: EXPECTED :Required log message:'RDKB_REBOOT:docsDevResetNow'for device, 'RDKB_REBOOT: Docsis_SNMP_Reboot request received, rebooting device' in /rdklogs/logs/SecConsole.txt.0 /nvram2/logs/SecConsole_lastreboot.txt.0 for arm devices and 'RDKB_REBOOT: SNMP Reboot request received, rebooting device' for fiber devices and 'RDKB_REBOOT: Reboot triggered by SNMP' for other devices should be present in Consolelog.txt.0/nvram2/logs/Consolelog.txt.0.");
	    LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.verifyTelemetryMarkerForDeviceRebootInitiatedBySnmpDocDevMib(settop,
			tapEnv);
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Required log message is present in Device Logs.");
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(settop, testCaseId, stepNumber, status, errorMessage, true);
	    } else {
		errorMessage = "Logging didn't appear.Marking the step as NA";
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(settop, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	} catch (Exception exception) {
	    errorMessage = errorMessage + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, settop, testCaseId, stepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("####################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Remove tmp files");
	    LOGGER.info("POST-CONDITION 1 : ACTION : Remove /tmp/Consolelog.txt");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : Files removed successfully");
	    LOGGER.info("####################################################################");
	    status = false;
	    errorMessage = "Failed to remove /tmp/Consolelog.txt file";
	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, settop,
		    BroadBandCommandConstants.FILE_PATH_TMP_CONSOLE_LOG);
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : File removed successfully");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-REBOOT-5001");
    }
}
