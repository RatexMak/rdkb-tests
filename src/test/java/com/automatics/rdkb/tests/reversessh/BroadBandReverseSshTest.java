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
package com.automatics.rdkb.tests.reversessh;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.reversessh.BroadBandReverseSshUtils;

public class BroadBandReverseSshTest extends AutomaticsTestBase {

    /**
     * Test to validate reverse SSH on bridge mode
     * 
     * <ol>
     * <li>Enable bridge mode via webpa</li>
     * <li>verify whether Bridge mode enabled using SNMP</li>
     * <li>Validate that brlan0 interface is down</li>
     * <li>Set Reverse ssh arguments using WebPA parameters Use below value to set -
     * "idletimeout=300;revsshport=\"portToUse\";sshport=22;user=webpa_user01;host=\"jump server machineIp\"".</li>
     * <li>Check reverse ssh connection status using WebPA get request</li>
     * <li>Attempt reverse SSH connection for the device with ecm mac id and verify log message in PARODUSlog.txt.0</li>
     * <li>Check reverse ssh connection status using WebPA get request after connecting to device.</li>
     * <li>Stop reverse ssh trigger from WebPA request and verify log message in PARODUSlog.txt.0</li>
     * <li>Check reverse ssh connection status using WebPA get request after closing the connecting</li>
     * <li>Attempt reverse SSH connection for the device with ecm mac id, even when reverse SSH trigger is stopped.</li>
     * <li>Check reverse ssh connection status using WebPA get request</li>
     * </ol>
     * 
     * @author SATHURYA RAVI
     * @refactor Athira
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-BRIDGEMODE-REVERSE-1001")
    public void testReverseSshConnectionOnBridgeMode(Dut device) {

	String testCaseId = "TC-RDKB-BRIDGEMODE-REVERSE-001";
	int stepNumber = 1;
	String stepNum = null;
	boolean status = false;
	String errorMessage = null;
	String response = null;

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-BRIDGEMODE-REVERSE-1001");
	    LOGGER.info(
		    "TEST DESCRIPTION: Check for the file startTunnel.sh in the folder  /lib/rdk/  Stop reverse ssh trigger from WebPA request .");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Enable bridge mode via webpa ");
	    LOGGER.info("2. verify whether Bridge mode enabled using SNMP");
	    LOGGER.info("3. Validate that brlan0 interface is down");
	    LOGGER.info("4. Set Reverse ssh arguments using WebPA parameters. "
		    + "  Use below value to set -\"idletimeout=300;revsshport=\"portToUse\""
		    + ";sshport=22;user=webpa_user01;host=\"jump server machineIp\"\".");
	    LOGGER.info("5. Stop reverse ssh trigger from WebPA request and verify log message in PARODUSlog.txt.0  ");
	    LOGGER.info("6. Check reverse ssh connection status using WebPA get request");
	    LOGGER.info("7. Attempt reverse SSH connection for the device with ecm mac id and verify log"
		    + " message in PARODUSlog.txt.0");
	    LOGGER.info("8. Check reverse ssh connection status using WebPA get request after connecting to device.");
	    LOGGER.info("9. Stop reverse ssh trigger from WebPA request and verify log message in PARODUSlog.txt.0  ");
	    LOGGER.info(
		    "10. Check reverse ssh connection status using WebPA get request after closing the connecting.");
	    LOGGER.info("11. Attempt reverse SSH connection for the device with ecm mac id, even when reverse"
		    + " SSH trigger is stopped.");
	    LOGGER.info("12. Check reverse ssh connection status using WebPA get request .");

	    LOGGER.info("#######################################################################################");

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("PRE-CONDITION - STEP 1: DESCRIPTION: Verify webpa process is up and running.");
	    LOGGER.info("PRE-CONDITION - STEP 1: ACTION - Webpa processs is checked by executing webpa query to "
		    + "get serial number using object \"Device.DeviceInfo.SerialNumber\"");
	    LOGGER.info("PRE-CONDITION - STEP 1: EXPECTED - WEBPA PROCESS MUST BE UP & RUNNING.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    LOGGER.info("PRE-CONDITION - STEP 1: ACTUAL - WEBPA PROCESS IS UP & RUNNING STATUS IS " + status);
	    if (!status) {
		errorMessage = "WEBPA Process is not up & running; hence blocking the execution.";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION - STEP 2: DESCRIPTION: Check for the file startTunnel.sh in the folder  /lib/rdk/ ");
	    LOGGER.info("PRE-CONDITION - STEP 2: ACTION: execute command if -f /lib/rdk/startTunnel.sh on jump server");
	    LOGGER.info(
		    "PRE-CONDITION - STEP 2: EXPECTED: File startTunnel.sh should be present in the folder /lib/rdk/");
	    LOGGER.info(
		    "************************************************************************************************");
	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_START_TUNNEL_SSH);
	    LOGGER.info("PRE-CONDITION - STEP 2: ACTUAL - File startTunnel.sh is present in the folder /lib/rdk/: "
		    + status);
	    if (!status) {
		errorMessage = "File startTunnel.sh is not present in /lib/rdk/ folder";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("PRE-CONDITION - STEP 3: DESCRIPTION: Stopping reverse ssh connection. ");
	    LOGGER.info("PRE-CONDITION - STEP 3: ACTION: Stopping reverse ssh connection via webpa using object"
		    + " Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshTrigger ");
	    LOGGER.info("PRE-CONDITION - STEP 3: EXPECTED: Reverse SSH Connection must be stopped.");
	    LOGGER.info(
		    "************************************************************************************************");
	    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER,
		    BroadBandTestConstants.STRING_STOP, BroadBandTestConstants.CONSTANT_0);
	    LOGGER.info("PRE-CONDITION - STEP 3: ACTUAL - Stopped Reverse SSH Connection: " + status);
	    if (!status) {
		errorMessage = "Failed to close Reverse SSH connection using WebPA request";
		LOGGER.error(errorMessage);
		throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
	    }

	    stepNum = "s" + stepNumber;
	    status = false;
	    errorMessage = "Not able to set the value of " + BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS
		    + " to bridege-static";

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Enable Bridge mode using webpa ");
	    LOGGER.info("STEP " + stepNumber + ": ACTION: Web PA server response should contain the"
		    + " message as success.");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED: Value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry."
			    + "1.LanMode must be set to \"bridge-static\" successfully");
	    LOGGER.info(
		    "************************************************************************************************");

	    try {

		status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_ENABLE, BroadBandTestConstants.CONSTANT_0,
			BroadBandTestConstants.CONSTANT_BRIDGE_STATIC, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

		if (status) {
		    LOGGER.info("STEP " + stepNumber + ": ACTUAL: Value of "
			    + "Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode is set to"
			    + " \"bridge-static\" successfully");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL: " + errorMessage);
		}
	    } catch (TestException exception) {
		LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		errorMessage = errorMessage + exception.getMessage();
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "s" + stepNumber;
	    status = false;
	    errorMessage = "Unable to check the bridge mode status via SNMP";
	    long startTime = System.currentTimeMillis();

	    LOGGER.info("*********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION:Verify the Bridge mode status via SNMP using snmp oid "
		    + "1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
	    LOGGER.info("STEP " + stepNumber + ": ACTION:Execute snmp get on oid 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Device should return the current mode and the value should be '1',"
		    + " that means it is in bridge mode.");
	    LOGGER.info("*********************************************************************************");
	    do {
		response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
			BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_ONE);
		if (!status) {
		    LOGGER.info("Waiting for 30 seconds...");
		    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}
	    } while (!status
		    && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Attempt to enable bridge mode was successful ");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "brlan0 interface is still up";
	    startTime = System.currentTimeMillis();

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Validate that brlan0 interface is down ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute the command ,\"ifconfig brlan0\" on the gateway and validate the result");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: The interface should have a valid IP assigned");
	    LOGGER.info(
		    "************************************************************************************************");
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		do {

		    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.CMD_CHECK_INTERFACE_LAN);
		    status = !CommonMethods.isNotNull(response);

		    if (!status) {
			LOGGER.info("Waiting for 30 seconds....");
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    }

		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.SIX_MINUTE_IN_MILLIS);

		if (status) {
		    LOGGER.info("STEP " + stepNumber + ": ACTUAL : brlan0 interface is down ");
		} else {
		    LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		}

		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		errorMessage = "skippig brlan0 bridge mode validations as it is a Fibre device";
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to set reverse ssh arguments via webpa";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Set Reverse ssh arguments using WebPA parameters ");
	    LOGGER.info("STEP " + stepNumber + ": ACTION: Execute the WebPA command to set Device.DeviceInfo.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: WebPA command should respond with success message");
	    LOGGER.info(
		    "************************************************************************************************");

	    // Jump server with Reverse ssh utility configured is added in
	    // stb.props file. Reverse ssh connection will
	    // be established from that jump server.
	    String reverseSshJumpServer = BroadbandPropertyFileHandler.getReverseSshJumpServer();
	    status = false;
	    if (CommonMethods.isNotNull(reverseSshJumpServer)) {
		// WebPA parameter sample
		// "idletimeout=300;revsshport='portToUse';sshport=22;user=webpa_user01;host='"+
		// reverseSshJumpServer + "'".
		String webpaParameters = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.BROADBAND_REVERSE_SSH_ARGUMENTS_EXCEPT_HOST,
			BroadBandTestConstants.STRING_HOST, AutomaticsConstants.DELIMITER_EQUALS,
			BroadBandTestConstants.SINGLE_QUOTE + reverseSshJumpServer,
			BroadBandTestConstants.SINGLE_QUOTE);
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_ARGS,
			webpaParameters, BroadBandTestConstants.CONSTANT_0);
		errorMessage = "Unable to set reverse ssh arguments using WebPA parameter Device.DeviceInfo."
			+ "X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshArgs";
	    } else {
		errorMessage = BroadBandPropertyKeyConstants.PROPERTY_REVERSE_SSH_JUMP_SERVER
			+ " Reverse SSH Jump Server not specified in stb.properties. Skipping command execution.";
		throw new TestException(BroadBandTestConstants.PROPERTY_NOT_FOUND_ERROR + errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully set Reverse ssh arguments using WebPA parameters");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to start reverse ssh trigger argument using WebPA";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Start reverse ssh trigger from WebPA request ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute the WebPA command to set Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: WebPA command should respond with success message ");
	    LOGGER.info(
		    "************************************************************************************************");

	    boolean isFibreorBussinessClassDevice = DeviceModeHandler.isFibreDevice(device)
		    || DeviceModeHandler.isBusinessClassDevice(device);
	    LOGGER.info("IS Fibre device OR BussinessClass DEVICE: " + isFibreorBussinessClassDevice);
	    if (!isFibreorBussinessClassDevice) {
		LOGGER.info("Expected Result - WebPA command should respond with success message ");
		try {
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER, BroadBandTestConstants.STRING_START,
			    BroadBandTestConstants.CONSTANT_0);
		} catch (TestException testException) {
		    // Log & Suppress the exception
		    LOGGER.error(testException.getMessage());
		}
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Is reverse SSH trigger value set successfully using WebPA parameter : "
			+ BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER + " - " + status);
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    } else {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : THIS STEP IS NOT APPLICABLE FOR FIBRE DEVICE AND BUSSINESSCLASS DEVICES.");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Reverse ssh connection status is not INACTIVE";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Check reverse ssh connection status using WebPA get request. ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get request using parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshStatus");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: WebPA command should respond with success message and Value should be "
		    + "\"INACTIVE\", as we have not connected into the device.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_INACTIVE);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : WebPA command response for get request using  : "
		    + BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS + " is : " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Failed to establish reverse ssh connection. No response from the device";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Attempt reverse SSH connection for the device with ecm mac id and verify"
		    + " log message in PARODUSlog.txt.0 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute the below command in jump server sudo revstbssh_rdkb <ECM_MAC> "
		    + "After Successful connection establishment. Check for success reverse ssh log message "
		    + "in PARODUSlog.txt.0 file Command - tail -f  /rdklogs/logs/PARODUSlog.txt.0 | "
		    + "grep -i 'xOpsReverseSshStatus\",\"value\":\"ACTIVE\"'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Device should be able to connect and response should be \"root\" and "
		    + "log message 'xOpsReverseSshStatus\",\"value\":\"ACTIVE\"'  for successful reverse "
		    + "ssh connectiono should be present in PARODUSlog.txt.0");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandReverseSshUtils.establishReverseSshConnectionInBroadBandDevice(device);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Is Reverse ssh connection established " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Reverse ssh connection status is not ACTIVE";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Check reverse ssh connection status using WebPA get request. ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get request using below commands. using parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshStatus");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: WebPA command should respond with success message and Value should be "
		    + "\"ACTIVE\", as we have not connected into the device.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_ACTIVE);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : WebPA command response is " + status);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Unable to stop reverse ssh trigger argument using WebPA";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION: Stop reverse ssh trigger from WebPA request ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute the WebPA command to set Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED: WebPA command should respond with success message ");
	    LOGGER.info(
		    "************************************************************************************************");

	    LOGGER.info("Expected Result - WebPA command should respond with success message ");
	    try {
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER,
			BroadBandTestConstants.STRING_STOP, BroadBandTestConstants.CONSTANT_0);
	    } catch (TestException testException) {
		// Log & Suppress the exception
		LOGGER.error(testException.getMessage());
	    }
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTUAL : Is reverse SSH trigger value set successfully using WebPA parameter "
		    + BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER + " - " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Reverse ssh connection status is not INACTIVE";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Check reverse ssh connection status using WebPA get request. ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get request using parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshStatus");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: WebPA command should respond with success message and Value should be "
		    + "\"INACTIVE\", as we have not connected into the device.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_INACTIVE);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : WebPA command response is : " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Failed to establish reverse ssh connection. No response from the device";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Attempt reverse SSH connection for the device with ecm mac id and verify"
		    + " log message in PARODUSlog.txt.0 ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute the below command in jump server sudo revstbssh_rdkb <ECM_MAC> "
		    + "After Successful connection establishment. Check for success reverse ssh log message "
		    + "in PARODUSlog.txt.0 file Command - tail -f  /rdklogs/logs/PARODUSlog.txt.0 | "
		    + "grep -i 'xOpsReverseSshStatus\",\"value\":\"ACTIVE\"'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: Device will automatically start reverse ssh trigger and will connect.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandReverseSshUtils.establishReverseSshConnectionInBroadBandDevice(device);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Is Reverse ssh connection established " + status);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info(
		    "************************************************************************************************");

	    ++stepNumber;
	    stepNum = "S" + stepNumber;
	    status = false;
	    errorMessage = "Reverse ssh connection status is not ACTIVE";
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION: Check reverse ssh connection status using WebPA get request. ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION: Execute webpa get request using parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshStatus");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED: WebPA command should respond with success message and Value should be "
		    + "\"ACTIVE\", as we have not connected into the device.");
	    LOGGER.info(
		    "************************************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_ACTIVE);

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Webpa get response using WebPA parameter "
		    + BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS + " - " + status);
	    LOGGER.info(
		    "************************************************************************************************");

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING REVERSE SSH CONNECTION : " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("POST CONDITION: STEP 1 : DESCRIPTION: Stop reverse ssh trigger from WebPA request ");
	    LOGGER.info(
		    "POST CONDITION: STEP 1 : ACTION: Execute the WebPA command to set value of parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt. to stop reverse ssh trigger from WebPA request");
	    LOGGER.info(
		    "************************************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.STRING_STOP);
	    LOGGER.info("POST CONDITION: STEP 1 ACTUAL :Reverse ssh connection "
		    + (status ? "closed successfully" : "not closed. Failed."));

	    LOGGER.info(
		    "************************************************************************************************");
	    LOGGER.info("POST CONDITION: STEP 2: DESCRIPTION: Disable Bridge mode using webpa ");
	    LOGGER.info("POST CONDITION: STEP 2:: ACTION: WebPA server response should contain the"
		    + " message as success.");
	    LOGGER.info(
		    "POST CONDITION: STEP 2: EXPECTED: Value of Device.X_CISCO_COM_DeviceControl.LanManagementEntry."
			    + "1.LanMode must be set to \"router\" successfully");
	    LOGGER.info(
		    "************************************************************************************************");

	    try {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_BRIDGE_MODE_STATUS, WebPaDataTypes.STRING.getValue(),
			BroadBandTestConstants.LAN_MANAGEMENT_MODE_ROUTER);
		if (status) {
		    LOGGER.info("POST CONDITION: STEP 2: ACTUAL: Value of "
			    + "Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode is set to"
			    + " \"router\" successfully");
		} else {
		    LOGGER.error("POST CONDITION: STEP 2: ACTUAL: " + errorMessage);
		}
	    } catch (TestException exception) {
		LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		errorMessage = errorMessage + exception.getMessage();
	    }

	    LOGGER.info("**********************************************************************************");

	    if (status) {

		LOGGER.info("*********************************************************************************");
		LOGGER.info("POST CONDITION: STEP 3: DESCRIPTION:Verify the Bridge mode status via SNMP using snmp oid "
			+ "1.3.6.1.4.1.17270.50.2.2.2.1.1.32");
		LOGGER.info("POST CONDITION: STEP 3: ACTION:Execute snmp get on oid 1.3.6.1.4.1.17270.50.2.3.2.1.1.32");
		LOGGER.info(
			"POST CONDITION: STEP 3: EXPECTED: Device should return the current mode and the value should be '1',"
				+ " that means it is in bridge mode.");
		LOGGER.info("*********************************************************************************");

		long startTime = System.currentTimeMillis();

		do {
		    response = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getOid(),
			    BroadBandSnmpMib.ENABLE_DISABLE_BRIDGE_MODE.getTableIndex());
		    status = CommonMethods.isNotNull(response)
			    && response.equalsIgnoreCase(BroadBandTestConstants.STRING_VALUE_TWO);
		    if (!status) {
			LOGGER.info("Waiting for 30 seconds...");
			tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		    }
		} while (!status
			&& (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS);

		if (status) {
		    LOGGER.info("POST CONDITION: STEP 3: ACTUAL : Attempt to enable bridge mode was successful ");
		} else {
		    LOGGER.error("POST CONDITION: STEP 3: ACTUAL : " + errorMessage);
		}

		LOGGER.info("**********************************************************************************");

	    }
	}
	LOGGER.info("ENDIND TEST CASE: TC-RDKB-BRIDGEMODE-REVERSE-1001");

    }

    /**
     * Test Case # 1: Verify Reverse SSH connection.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION : Reboot the device .Check for the file startTunnel.sh in the folder /lib/rdk/ . Stop reverse
     * ssh trigger from WebPA request.</li>
     * <li>S1) Set Reverse ssh arguments using WebPA parameters.</li>
     * <li>S2) Start reverse ssh trigger from WebPA request.</li>
     * <li>S3) Check reverse ssh connection status using WebPA get request.</li>
     * <li>S4) Attempt reverse SSH connection for the device with ecm mac id and verify log message in
     * PARODUSlog.txt.0.</li>
     * <li>S5) Check reverse ssh connection status using WebPA get request after connecting to device.</li>
     * <li>S6) Stop reverse ssh trigger from WebPA request.</li>
     * <li>S7) Check reverse ssh connection status using WebPA get request after closing the connecting.</li>
     * <li>S8) Attempt reverse SSH connection for the device with ecm mac id, even when reverse SSH trigger is
     * stopped</li>
     * <li>S9) Check reverse ssh connection status using WebPA get request after connecting to device.</li>
     * </ol>
     * 
     * @author Praveen Kumar P
     * @refactor Alan_Bivera
     * 
     * @param Dut
     *            {@link device}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-REVSSH-1001")
    public void testReverseSshConnection(Dut device) {
	String testCaseId = "TC-RDKB-REVSSH-001";
	String stepNumber = "s";
	int count = 1;
	if (DeviceModeHandler.isDSLDevice(device)) {
	    while (count <= BroadBandTestConstants.CONSTANT_9) {
		stepNumber = "S" + count;
		tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			"NOT APPLICABLE SINCE MAP-T LINE PRESENT", false);
		count++;
	    }
	} else {
	    boolean status = false;
	    String errorMessage = null;
	    try {

		/**
		 * PRE-CONDITION : 1)WebPA Process 2)Check for the file startTunnel.sh in the folder /lib/rdk/ 3) Stop
		 * reverse ssh trigger from WebPA request.
		 */
		LOGGER.info("STARTING TEST CASE : TC-RDKB-REVSSH-1001");
		LOGGER.info("**********************************************************************************");
		LOGGER.info("PRE-CONDITION - STEP 1: VERIFY WEBPA PROCESS IS UP & RUNNING.");
		LOGGER.info("PRE-CONDITION - STEP 1: EXPECTED - WEBPA PROCESS MUST BE UP & RUNNING.");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		LOGGER.info("PRE-CONDITION - STEP 1: EXPECTED - WEBPA PROCESS IS UP & RUNNING: " + status);
		if (!status) {
		    errorMessage = "WEBPA Process is not up & running; hence blocking the execution.";
		    LOGGER.error(errorMessage);
		    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
		}

		LOGGER.info("PRE-CONDITION - STEP 2: Check for the file startTunnel.sh in the folder  /lib/rdk/ ");
		LOGGER.info(
			"PRE-CONDITION - STEP 2: EXPECTED - File startTunnel.sh should be present in the folder /lib/rdk/");
		status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_START_TUNNEL_SSH);
		LOGGER.info("PRE-CONDITION - STEP 2: ACTUAL - File startTunnel.sh is present in the folder /lib/rdk/: "
			+ status);
		if (!status) {
		    errorMessage = "File startTunnel.sh is not present in /lib/rdk/ folder";
		    LOGGER.error(errorMessage);
		    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
		}

		LOGGER.info("PRE-CONDITION - STEP 3: Stopping reverse ssh connection. ");
		LOGGER.info("PRE-CONDITION - STEP 3: EXPECTED - Reverse SSH Connection must be stopped.");
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER,
			BroadBandTestConstants.STRING_STOP, BroadBandTestConstants.CONSTANT_0);
		LOGGER.info("PRE-CONDITION - STEP 3: ACTUAL - Stopped Reverse SSH Connection: " + status);
		if (!status) {
		    errorMessage = "Failed to close Reverse SSH connection using WebPA request";
		    LOGGER.error(errorMessage);
		    throw new TestException(BroadBandTestConstants.PRE_CONDITION_ERROR + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 1 : Set Reverse ssh arguments using WebPA parameters
		 */
		stepNumber = "s1";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Step 1 : Set Reverse ssh arguments using WebPA parameters ");
		LOGGER.info("Expected Result - WebPA command should respond with success message ");
		LOGGER.info("**********************************************************************************");
		// Jump server with Reverse ssh utility configured is added in stb.props file. Reverse ssh connection
		// will
		// be established from that jump server.
		String reverseSshJumpServer = BroadbandPropertyFileHandler.getReverseSshJumpServer();
		if (CommonMethods.isNotNull(reverseSshJumpServer)) {
		    String webpaParameters = BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandTestConstants.BROADBAND_REVERSE_SSH_ARGUMENTS_EXCEPT_HOST,
			    BroadBandTestConstants.STRING_HOST, AutomaticsConstants.DELIMITER_EQUALS,
			    BroadBandTestConstants.SINGLE_QUOTE + reverseSshJumpServer,
			    BroadBandTestConstants.SINGLE_QUOTE);
		    status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_ARGS,
			    webpaParameters, BroadBandTestConstants.CONSTANT_0);
		    errorMessage = "Unable to set reverse ssh arguments using WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshArgs";
		} else {
		    errorMessage = BroadBandPropertyKeyConstants.PROPERTY_REVERSE_SSH_JUMP_SERVER
			    + " Reverse SSH Jump Server not specified in stb.properties. Skipping command execution.";
		    throw new TestException(BroadBandTestConstants.PROPERTY_NOT_FOUND_ERROR + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 2 : Start reverse ssh trigger from WebPA request
		 */
		stepNumber = "s2";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Step 2 : Start reverse ssh trigger from WebPA request ");
		LOGGER.info("**********************************************************************************");
		boolean isFiberorBuisnessDevice = DeviceModeHandler.isFibreDevice(device)
			|| DeviceModeHandler.isBusinessClassDevice(device);
		LOGGER.info("Is Fibre or Business Class device: " + isFiberorBuisnessDevice);
		if (!isFiberorBuisnessDevice) {
		    LOGGER.info("Expected Result - WebPA command should respond with success message ");
		    try {
			status = BroadBandWiFiUtils.setWebPaParams(device,
				BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER, BroadBandTestConstants.STRING_START,
				BroadBandTestConstants.CONSTANT_0);
		    } catch (TestException testException) {
			// Log & Suppress the exception
			LOGGER.error(testException.getMessage());
		    }
		    LOGGER.info("Is reverse SSH trigger value set successfully using WebPA parameter "
			    + BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER + " - " + status);
		    errorMessage = "Unable to set reverse ssh trigger argument using WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshTrigger";
		    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		} else {
		    LOGGER.info("Expected Result - THIS STEP IS NOT APPLICABLE FOR FIBRE AND BUISNESS CLASS DEVICES.");
		    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 3 : Check reverse ssh connection status using WebPA get request.
		 */
		stepNumber = "s3";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Step 3 : Check reverse ssh connection status using WebPA get request.");
		LOGGER.info(
			"Expected Result - WebPA command should respond with success message and Value should be \"INACTIVE\", as we have not connected into the device. ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_INACTIVE);
		errorMessage = "Reverse ssh connection status is not INACTIVE";
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 4 : Attempt reverse SSH connection for the device with ecm mac id and verify log message in
		 * PARODUSlog.txt.0.
		 */
		stepNumber = "s4";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"Step 4 : Attempt reverse SSH connection for the device with ecm mac id and verify log message in PARODUSlog.txt.0. ");
		LOGGER.info("Expected Result - Device should be able to connect. ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandReverseSshUtils.establishReverseSshConnectionInBroadBandDevice(device);
		LOGGER.info("Is Reverse ssh connection established " + status);
		errorMessage = "Failed to establish reverse ssh connection. No response from the device";

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 5 : Check reverse ssh connection status using WebPA get request after connecting to device..
		 */
		stepNumber = "s5";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"Step 5 : Check reverse ssh connection status using WebPA get request after connecting to device ");
		LOGGER.info(
			"Expected Result - WebPA command should respond with success message and Value should be \"ACTIVE\", ss reverse ssh connection is established. ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_ACTIVE);
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 6 : Stop reverse ssh trigger from WebPA request
		 */
		stepNumber = "s6";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info("Step 6 : Stop reverse ssh trigger from WebPA request ");
		LOGGER.info("Expected Result - WebPA command should respond with success message ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER,
			BroadBandTestConstants.STRING_STOP, BroadBandTestConstants.CONSTANT_0);
		errorMessage = "Unable to set reverse ssh trigger argument using WebPA parameter Device.DeviceInfo.X_RDKCENTRAL-COM_xOpsDeviceMgmt.ReverseSSH.xOpsReverseSshTrigger";

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 7 : Check reverse ssh connection status using WebPA get request.
		 */
		stepNumber = "s7";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"Step 7 : Check reverse ssh connection status using WebPA get request after closing the connecting.");
		LOGGER.info(
			"Expected Result - WebPA command should respond with success message and Value should be \"INACTIVE\".");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_INACTIVE);
		errorMessage = "Reverse ssh connection status is not INACTIVE";
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 8 : Attempt reverse SSH connection for the device with ecm mac id, even when reverse SSH trigger
		 * is stopped.
		 */
		stepNumber = "s8";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"Step 8 : Attempt reverse SSH connection for the device with ecm mac id, even when reverse SSH trigger is stopped.");
		LOGGER.info("Expected Result - Device will automatically start reverse ssh trigger and will connect.");
		LOGGER.info("**********************************************************************************");
		status = BroadBandReverseSshUtils.establishReverseSshConnectionInBroadBandDevice(device);
		LOGGER.info("Is Reverse ssh connection established " + status);
		errorMessage = "Failed to establish reverse ssh connection. No response from the device";

		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
		LOGGER.info("**********************************************************************************");
		/**
		 * Step 9 : Check reverse ssh connection status using WebPA get request after connecting to device..
		 */
		stepNumber = "s9";
		status = false;
		errorMessage = BroadBandTestConstants.EMPTY_STRING;
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"Step 9 : Check reverse ssh connection status using WebPA get request after connecting to device ");
		LOGGER.info(
			"Expected Result - WebPA command should respond with success message and Value should be \"ACTIVE\". ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_REVERSE_SSH_STATUS, BroadBandTestConstants.STRING_ACTIVE);
		errorMessage = "Reverse ssh connection status is not ACTIVE";
		tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
		LOGGER.info("**********************************************************************************");

	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING REVERSE SSH CONNECTION : " + errorMessage);
		CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status,
			errorMessage, true);
	    } finally {
		LOGGER.info("**********************************************************************************");
		LOGGER.info("POST-CONDITION : Stopping reverse ssh connection. ");
		LOGGER.info("**********************************************************************************");
		status = BroadBandWiFiUtils.setWebPaParams(device, BroadBandWebPaConstants.WEBPA_REVERSE_SSH_TRIGGER,
			BroadBandTestConstants.STRING_STOP, BroadBandTestConstants.CONSTANT_0);
		LOGGER.info("Reverse ssh connection " + (status ? "closed successfully" : "not closed. Failed."));
		LOGGER.info("**********************************************************************************");
		LOGGER.info("TEST CASE END : TC-RDKB-REVSSH-1001");
	    }
	}
    }

}
