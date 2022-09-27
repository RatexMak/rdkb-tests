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
package com.automatics.rdkb.tests.system;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandConnectedClientTestConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadBandSystemUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.cdl.BroadBandCodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.AutomaticsPropertyUtility;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaParameter;
import com.automatics.webpa.WebPaServerResponse;

public class BroadBandSystemTests extends AutomaticsTestBase {

    private static final Logger LOGGER = LoggerFactory.getLogger(BroadBandSystemTests.class);

    /** Hard-coded the response expected for query send to external server */
    private static final String CONST_EXPECTED_RESPONSE = "HTTP/1.1 200 OK";

    /**
     * Command to list files order by creation date. Most recent file will be shown first
     */
    private static final String CMD_LIST_FILES_ORDER_BY_DATE = "ls -t ";

    /** Command to get the head file from a list of files */
    private static final String CMD_GET_HEAD_FILE = "head -n1";

    /**
     * 
     * <li>STEP 1: Verify the telnetd & telnet file is not available in ATOM Console</li>
     * <li>STEP 2: Verify the telnet connection from ARM to ATOM is not working</li>
     * <li>STEP 3: Verify the telnet connection to CM IP</li>
     * <li>STEP 4: Verify the telnet connection to WAN IP</li>
     * <li>STEP 5: Verify the telnet connection to MTA IP</li>
     * </ol>
     * 
     * @author ArunKumar Jayachandran
     * @refactor yamini.s
     * @param device
     *            The device to be used.
     * @throws Exception
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1021")
    public void testToVerifyTelnetDaemonRemoval(Dut device) {
	// Test step number
	String step = "s1";
	// stores the test ID
	String testId = "TC-RDKB-SYSTEM-021";
	// execution status
	boolean status = false;
	// error message
	String errorMessage = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1021");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the telnetd & telnet file is not available in ATOM Console");
	LOGGER.info("2. Verify the telnet connection from ARM to ATOM is not working");
	LOGGER.info("3. Verify the telnet connection to CM IP");
	LOGGER.info("4. Verify the telnet connection to WAN IP");
	LOGGER.info("5. Verify the telnet connection to MTA IP");
	LOGGER.info("#######################################################################################");
	try {
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the telnetd & telnet file is not available in ATOM Console");
	    LOGGER.info("STEP 1: ACTION :Execute 'find' command in atom console to check the availability ");
	    LOGGER.info("STEP 1: EXPECTED : telnetd & telnet file should not be available in ATOM Console ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "telnetd file is available in ATOM side";
	    status = BroadBandSystemUtils.verifytelnetFileAvailabilityInAtomConsole(tapEnv, device,
		    BroadBandCommandConstants.CMD_TELNETD);
	    LOGGER.info("Verification of telnetd file non availability in ATOM side: " + status);
	    if (status) {
		errorMessage = "telnet file is available in ATOM side";
		status = BroadBandSystemUtils.verifytelnetFileAvailabilityInAtomConsole(tapEnv, device,
			BroadBandCommandConstants.CMD_TELNET);
		LOGGER.info("Verification of telnet file non availability in ATOM side: " + status);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1 ACTUAL : Verification of telnetd and telnet file non availability in ATOM side is successful");
	    } else {
		LOGGER.error("STEP 1 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, true);

	    step = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the telnet connection from ARM to ATOM is not working");
	    LOGGER.info("STEP 2: ACTION :Validate telnet connection for both telnet and ssh to ATOM console ");
	    LOGGER.info("STEP 2: EXPECTED : telnet connection from ARM to ATOM should not be working ");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Verification of telnet proccess using ps command");
	    errorMessage = "telnet process is available";
	    String pidOfTelnetProcess = BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
		    BroadBandCommandConstants.CMD_TELNET);
	    status = CommonMethods.isNull(pidOfTelnetProcess);
	    errorMessage = "telnet process is available";
	    LOGGER.info("Verification of telnet proccess non availability status: " + status);
	    if (status) {
		pidOfTelnetProcess = BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
			BroadBandCommandConstants.CMD_TELNETD);
		status = CommonMethods.isNull(pidOfTelnetProcess);
		errorMessage = "telnetd process is available";
		LOGGER.info("Verification of telnetd proccess non availability status: " + status);
	    }
	    LOGGER.info("Collecting the ATOM_IP from device.properties file");
	    // to store ATOM IP response from device.properties file
	    String atomIp = CommonMethods.getAtomServerIp(device, tapEnv);
	    errorMessage = "Unable to get ATOM IP From the device.";
	    LOGGER.info("ATOM_IP Address from device.properties file:" + atomIp);
	    if (status && CommonMethods.isNotNull(atomIp)) {
		status = !BroadBandSystemUtils.checkConnectionToIpAddress(tapEnv, device,
			BroadBandCommandConstants.CMD_TELNET, atomIp);
		errorMessage = "telnet to ATOM from ARM is working";
		LOGGER.info("Telnet ATOM from ARM negative response status: " + status);
	    }
	    if (status) {
		errorMessage = "ssh connection to ATOM console is possible";
		status = !BroadBandSystemUtils.checkConnectionToIpAddress(tapEnv, device,
			BroadBandCommandConstants.CMD_SSH, atomIp);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2 ACTUAL : Verify of the telnet connection from ARM to ATOM is not working is successful");
	    } else {
		LOGGER.error("STEP 2 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    step = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify the telnet connection to CM IP");
	    LOGGER.info("STEP 3: ACTION :Validate telnet connection to Wan0 interface ");
	    LOGGER.info("STEP 3: EXPECTED : telnet connection to Wan0 interface should not be working ");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Collecting the CM IP using ifconfig wan0");
	    errorMessage = "Able to telnet to wan0 interface";
	    // verify telnet connection to wan0 ipv6 interface
	    status = !(BroadBandSystemUtils.verifyTelnetConnectionToInterface(tapEnv, device,
		    BroadBandTestConstants.INTERFACE_NAME_WAN0));
	    if (status) {
		LOGGER.info("STEP 3 ACTUAL : telnet connection to wan0 interface should not be working");
	    } else {
		LOGGER.error("STEP 3 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    step = "s4";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the telnet connection to WAN IP");
	    LOGGER.info("STEP 4: ACTION :Validate telnet connection to erouter0 interface ");
	    LOGGER.info("STEP 4: EXPECTED : telnet connection to erouter0 interface should not be working ");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Collecting the WAN IP using ifconfig erouter0");
	    errorMessage = "Able to telnet to erouter0 interface";
	    // verify telnet connection to erouter0 ipv6 interface
	    status = !(BroadBandSystemUtils.verifyTelnetConnectionToInterface(tapEnv, device,
		    BroadBandTestConstants.INTERFACE_NAME_EROUTER0));
	    if (status) {
		LOGGER.info("STEP 4 ACTUAL : telnet connection to erouter0 interface should not be working");
	    } else {
		LOGGER.error("STEP 4 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);

	    step = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify the telnet connection to MTA IP");
	    LOGGER.info("STEP 5: ACTION :Validate telnet connection to mta0 interface ");
	    LOGGER.info("STEP 5: EXPECTED : telnet connection to mta0 interface should not be working ");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Collecting the MTA IP using ifconfig mta0");
	    errorMessage = "Able to telnet to mta0 interface";
	    // verify telnet connection to mta0 ipv6 interface
	    status = !(BroadBandSystemUtils.verifyTelnetConnectionToInterface(tapEnv, device,
		    BroadBandTestConstants.INTERFACE_NAME_MTA0));
	    if (status) {
		LOGGER.info("STEP 5 ACTUAL : telnet connection to mta0 interface should not be working");
	    } else {
		LOGGER.error("STEP 5 ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, step, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, step, status, errorMessage, false);
	}
    }

    /**
     *
     * Test Case : Verify logging for Harvester and LMLite reports.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Perform factory reset on a device.</li>
     * <li>PRE-CONDITION 2 : Reactivate the Device.</li>
     * <li>Step 1 : Verify parodus process is running on ARM side.</li>
     * <li>Step 2 : Verify enabling the Interface Device Wifi via webpa command.</li>
     * <li>Step 3 : Verify lmlite process is running on ARM side.Kill the lmlite process and verify new process comes
     * up.</li>
     * <li>Step 4 : Verify log information "LMLite: Init for parodus Success..!!" is available in LM.txt.0 log.</li>
     * <li>Step 5 : Verify enabling the Network Device Traffic via webpa command.</li>
     * <li>Step 6 : Verify harvester process is running on ATOM side.Kill the harvester process and verify new process
     * comes up.</li>
     * <li>Step 7 : Verify log information "harvester_initialized created" is available in Harvesterlog.txt.0 log.</li>
     * <li>POST-CONDITION 1 : Verify device is reactivated,if already not activated.</li>
     * <li>POST-CONDITION 2 : Revert the default interface devices wifi and network devices traffic mode via webpa.</li>
     * 
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Elangovan
     * @refactor said.h
     *
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5002")
    public void testToVerifyHarvesterAndLmliteLog(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-502";
	String errorMessage = null;
	boolean status = false;
	int stepNumber = 1;
	String step = "S" + stepNumber;
	boolean isFactoryResetDone = false;
	boolean isReactivated = false;
	int preConStepNumber = 1;
	int postConStepNumber = 1;
	try {
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5002");
	    LOGGER.info("TEST DESCRIPTION: Verify logging for Harvester and LMLite reports.");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(" PRE CONDITION 1 : Perform factory reset on a device.");
	    LOGGER.info(" PRE CONDITION 2 : Reactivate the Device.");
	    LOGGER.info("Step 1 : Verify parodus process is running on ARM side.");
	    LOGGER.info("Step 2 : Verify enabling the Interface Devices Wifi via webpa command.");
	    LOGGER.info(
		    "Step 3 : Verify lmlite process is running on ARM side. Kill the lmlite process and verify new process comes up.");
	    LOGGER.info(
		    "Step 4 : Verify log information 'LMLite: Init for parodus Success..!!'  is available in LM.txt.0 log");
	    LOGGER.info("Step 5 : Verify enabling the Network Device Traffic  via webpa command.");
	    LOGGER.info(
		    "Step 6 : Verify harvester process is running on ATOM side. Kill the harvester process and verify new process comes up.");
	    LOGGER.info(
		    "Step 7 : Verify log information 'harvester_initialized created'  is available in Harvesterlog.txt.0 log");
	    LOGGER.info(" POST-CONDITION 1 : Verify device is reactivated,if already not activated.");
	    LOGGER.info(
		    " POST-CONDITION 2 : Revert the default interface devices wifi and network devices traffic mode via webpa.");
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
	     */
	    isFactoryResetDone = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    preConStepNumber);
	    /**
	     * PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE
	     */
	    preConStepNumber++;
	    isReactivated = BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
		    preConStepNumber);
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1: VERIFY PARODUS PROCESS IS RUNNING ON ARM SIDE
	     */
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY PARODUS PROCESS IS RUNNING ON ARM SIDE.");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : EXECUTE COMMAND: ps | grep parodus");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: PARODUS PROCESS DETAILS SHOULD BE DISPLAYED AS (/USR/BIN/PARODUS).");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "PARODUS PROCESS DETAILS IS NOT DISPLAYED";
	    String response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.PARODUS_PROCESS_OUTPUT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : PARODUS PROCESS IS RUNNING ON ARM SIDE");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 2: VERIFY ENABLING THE INTERFACE DEVICES WIFI VIA WEBPA COMMAND
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY ENABLING THE INTERFACE DEVICES WIFI VIA WEBPA COMMAND.");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE WEBPA COMMAND : Device.X_RDKCENTRAL-COM_Report.InterfaceDevicesWifi.Enabled");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: ENABLING INTERFACE DEVICES WIFI VIA WEBPA SHOULD BE SUCCESSFUL");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO ENABLE THE INTERFACE DEVICES WIFI VIA WEBPA COMMAND";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + " : ACTUAL : INTERFACE DEVICES WIFI ENABLED SUCCESSFULLY VIA WEBPA.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 3 : VERIFY CCSPLMLITE PROCESS IS RUNNING ON ATOM SIDE. KILL THE CCSPLMLITE PROCESS AND VERIFY NEW
	     * PROCESS COMES UP.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY CCSPLMLITE PROCESS IS RUNNING ON ATOM SIDE. KILL THE CCSPLMLITE PROCESS AND VERIFY NEW PROCESS COMES UP.");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : EXECUTE COMMAND: KILL -11 <PID_OF_CCSPLMLITE_PROCESS>");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: PID OF CCSPLMLITE SHOULD RETURN NEW VALUE THAN EXISTING PID.");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY CCSPLMLITE PROCESS IS RUNNING ON ATOM SIDE";
	    status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_LMLITE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : CCSPLMLITE PROCESS RESTARTED WITH NEW PID.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * SETP 4 : VERIFY LOG INFORMATION "LMLite: Init for parodus Success..!!" IS AVAILABLE IN LM.TXT.0 LOG
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY LOG INFORMATION 'LMLITE: INIT FOR PARODUS SUCCESS..!!'  IS AVAILABLE IN LM.TXT.0 LOG");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND:  grep -i 'LMLite: Init for parodus Success..!!' /rdklogs/logs/LM.txt.0 ");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: LOG INFORMATION 'LMLITE: INIT FOR PARODUS SUCCESS..!!' SHOULD BE AVAILABLE IN LM.TXT.0 LOG");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY THE 'LMLITE: INIT FOR PARODUS SUCCESS..!!' IN LM.TXT.0 LOG";
	    status = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcher(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_LMLITE_INTEGRATION, BroadBandCommandConstants.LOG_FILE_LMLITE,
		    BroadBandTraceConstants.LOG_MESSAGE_LMLITE_INTEGRATION
			    .replaceAll(BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING),
		    BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED THE LOG INFORMATION 'LMLITE: INIT FOR PARODUS SUCCESS..!!' IS AVAILABLE IN LM.TXT.0 LOG");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 5 : VERIFY ENABLING THE NETWORK DEVICE TRAFFIC VIA WEBPA COMMAND
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY ENABLING THE NETWORK DEVICE TRAFFIC VIA WEBPA COMMAND.");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE WEBPA COMMAND : Device.X_RDKCENTRAL-COM_Report.NetworkDevicesTraffic.Enabled");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: ENABLING NETWORK DEVICE TRAFFIC VIA WEBPA SHOULD BE SUCCESSFUL");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO ENABLE THE NETWORK DEVICE TRAFFIC VIA WEBPA COMMAND";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : NETWORK DEVICE TRAFIC ENABLED SUCCESSFULLY VIA WEBPA.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 6 : VERIFY HARVESTER PROCESS IS RUNNING ON ATOM SIDE. KILL THE HARVESTER PROCESS AND VERIFY NEW
	     * PROCESS COMES UP.
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY HARVESTER PROCESS IS RUNNING ON ATOM SIDE. KILL THE HARVESTER PROCESS AND VERIFY NEW PROCESS COMES UP.");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : EXECUTE COMMAND: KILL -11 <PID_OF_HARVESTER_PROCESS>");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: PID OF HARVERSTER SHOULD RETURN NEW VALUE THAN EXISTING PID.");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY HARVESTER PROCESS IS RUNNING ON ATOM SIDE";
	    status = BroadBandSystemUtils.killAndVerifyProcessRestarted(device, tapEnv,
		    BroadBandTestConstants.STRING_HARVESTER);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : HARVESTER PROCESS RESTARTED WITH NEW PID.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * SETP 7 : VERIFY LOG INFORMATION "HARVESTER_INITIALIZED CREATED" IS AVAILABLE IN HARVESTERLOG.TXT.0 LOG
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY LOG INFORMATION 'HARVESTER_INITIALIZED CREATED' IS AVAILABLE IN HARVESTERLOG.TXT.0 LOG.");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND:  grep -i 'harvester_initialized created' /rdklogs/logs/Harvesterlog.txt.0");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: LOG INFORMATION 'HARVESTER_INITIALIZED CREATED' SHOULD BE AVAILABLE IN HARVESTERLOG.TXT.0 LOG");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY THE 'HARVESTER_INITIALIZED CREATED' LOG IN HARVESTERLOG.TXT.0 LOG";
	    status = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcher(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INITIALIZATION,
		    BroadBandCommandConstants.FILE_HARVESTER_LOG,
		    BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INITIALIZATION
			    .replaceAll(BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.EMPTY_STRING),
		    BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED THE LOG INFORMATION 'HARVESTER_INITIALIZED CREATED' IS AVAILABLE IN HARVESTERLOG.TXT.0 LOG");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFING LOGS FOR HARVESTER AND LMLITE REPORTS : " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * POST-CONDITION 1 : DEVICE REACTIVATION
	     */
	    if (isFactoryResetDone && !isReactivated) {
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, isReactivated,
			postConStepNumber);
		postConStepNumber++;
	    }
	    /**
	     * POST CONDITION 2 : REVERT THE DEFAULT INTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC MODE VIA WEBPA.
	     */
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("POST-CONDITION " + postConStepNumber
		    + " : DESCRIPTION : REVERT THE DEFAULT INTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC MODE VIA WEBPA.");
	    LOGGER.info("POST-CONDITION " + postConStepNumber
		    + " : SET THE DEFAULT INTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC USING WEBPA");
	    LOGGER.info("POST-CONDITION " + postConStepNumber
		    + " : EXPECTED : MUST SET THE DEFAULT INTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC MODE");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "FAILED TO SET DEFAULT INTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC MODE";
	    List<WebPaParameter> webPaParameters = new ArrayList<WebPaParameter>();
	    WebPaParameter interfaceDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		    BroadBandWebPaConstants.WEBPA_INTERFACE_DEVICES_WIFI_REPORT, BroadBandTestConstants.FALSE,
		    WebPaDataTypes.BOOLEAN.getValue());
	    webPaParameters.add(interfaceDisabled);
	    WebPaParameter networkDisabled = BroadBandWebPaUtils.generateWebpaParameterWithValueAndType(
		    BroadBandWebPaConstants.WEBPA_NETWORK_DEVICES_TRAFFIC_WIFI_REPORT, BroadBandTestConstants.FALSE,
		    WebPaDataTypes.BOOLEAN.getValue());
	    webPaParameters.add(networkDisabled);
	    status = BroadBandWebPaUtils.setVerifyMultipleWebPAInPolledDuration(device, tapEnv, webPaParameters,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("POST-CONDITION " + postConStepNumber
			+ " : ACTUAL : SUCCESSFULLY SET DEFAULTINTERFACE DEVICES WIFI AND NETWORK DEVICES TRAFFIC MODE");
	    } else {
		LOGGER.info("POST-CONDITION " + postConStepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5002");
    }

    /** Hard-coded the maximum expected number of dnsmasq running instance */
    private static final String CONST_MAXIMUM_EXPECTED_DNS_MASQ_RUNNING_INSTANCE = "1";

    /**
     * Execute command systemctl status dnsmasq.service
     * <ol>
     * <li>Check ths status of dnsmasq service</li>
     * <li>Verify if process dnsmasq is running</li>
     * <li>Verify whether Zombie dnsmasq process is running</li>
     * <li>Verify SelfHeal.txt log file for the below error message. "dnsmasq is not running"</li>
     * <li>Kill the dnsmasq process</li>
     * <li>Verify whether any Zombie dnsmasq process is running</li>
     * <li>Restart the dnsmasq process.</li>
     * <li>Check for only one instance of dns is running</li>
     * <li>Verify SelfHeal.txt log file for the below error message. "dnsmasq is not running"</li>
     * <li>Check ths status of dnsmasq service</li>
     * </ol>
     */

    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1031")
    public void verifyZombieDnsMasqProcess(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SYSTEM-131";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1031");
	LOGGER.info("TEST DESCRIPTION: Execute command systemctl status dnsmasq.service");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Check the status of dnsmasq service ");
	LOGGER.info("2. Verify if process dnsmasq is running ");
	LOGGER.info("3. Verify whether Zombie dnsmasq process  is running ");
	LOGGER.info("4. Verify SelfHeal.txt log file for the below error message. \"dnsmasq is not running\"");
	LOGGER.info("5. Kill the dnsmasq process ");
	LOGGER.info("6. Verify whether any Zombie dnsmasq process  is running ");
	LOGGER.info("7. Restart the dnsmasq process. ");
	LOGGER.info("8. Check for only one instance of dns is running");
	LOGGER.info("9. Verify SelfHeal.txt log file for the below error message.\"dnsmasq is not running\"");
	LOGGER.info("10. Check ths status of dnsmasq service");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "dnsmasq service is not active currently";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Check ths status of dnsmasq service ");
	    LOGGER.info("STEP 1: ACTION : Execute command systemctl status dnsmasq.service");
	    LOGGER.info("STEP 1: EXPECTED : Command output should contain    Active: active (running)");
	    LOGGER.info("**********************************************************************************");

	    try {
		String expectedResponse = AutomaticsPropertyUtility
			.getProperty(BroadBandTestConstants.PROP_KEY_EXPECTED_RESPONSE);
		status = BroadBandSystemUtils.verifyDnsmasqSerivceStatus(device, tapEnv, expectedResponse);
	    } catch (Exception e) {
		status = false;
		errorMessage += "." + e.getMessage();
		LOGGER.error(e.getMessage());
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Verified that dnsmasq service is currently in Active state");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "The process dnsmasq is not running currently.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify if process dnsmasq is running ");
	    LOGGER.info("STEP 2: ACTION :  Execute the command   \"ps | grep dnsmasq |grep -v grep\" ");
	    LOGGER.info("STEP 2: EXPECTED : The output of the command should be as below"
		    + "12301 nobody    1976 S    dnsmasq -u nobody -q --clear-on-reload --bind-dynamic -"
		    + "-add-mac --add-cpe-id=abcdefgh --dhcp-authoritative -P 4096 -C /var/dnsmasq.conf      ");
	    LOGGER.info("**********************************************************************************");
	    String response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.COMMAND_TO_LIST_DNSMASQ_FROM_RUNNING_PROCESSES);
	    status = CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.STRING_DNSMASQ_PROCESS);
	    errorMessage += ".Actual Command Response : " + response;
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL :DNSMASQ process is currently running. ");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "A zombie dnsmasq process is currently running  in the device ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify whether Zombie dnsmasq process  is running ");
	    LOGGER.info("STEP 3: ACTION :  Execute the command   ps | grep dnsmasq |grep -v grep | grep \"Z\"");
	    LOGGER.info("STEP 3: EXPECTED : The output is expected to be null.No Zombie process should be running");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.COMMAND_TO_LIST_DNSMASQ_ZOMBIE_FROM_RUNNING_PROCESSES);
	    status = CommonMethods.isNull(response);
	    errorMessage += ".Actual Command Response : " + response;

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Verified that no Zombie dnsmasq process is running currently");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "The errormessage \"dnsmasq is not running\" is present in the Self heal logs ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify SelfHeal.txt log file for the below error message.\"dnsmasq is not running\"");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the command \"cat /rdklogs/logs/SelfHeal.txt.0 | grep \"dnsmasq is not running\"");
	    LOGGER.info("STEP 4: EXPECTED : The error message should not be present in the Selfheal log file");
	    LOGGER.info("**********************************************************************************");

	    String command = CommonMethods.concatStringUsingStringBuffer(CommonMethods.GREP_COMMAND,
		    AutomaticsConstants.DOUBLE_QUOTE, BroadBandTestConstants.STRING_DNSMASQ_NOT_RUNNING,
		    AutomaticsConstants.DOUBLE_QUOTE, AutomaticsConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.LOG_FILE_SELFHEAL);

	    response = tapEnv.executeCommandUsingSsh(device, command);
	    if (CommonMethods.isNull(response) && !CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_MATCHER_FIND_NO_SUCH_FILE)) {
		status = true;
	    } else {
		errorMessage += ".Obtained a non null response. The below logs are present in Self heal logs\n"
			+ response;
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : As expected ,the errormessage \"dnsmasq is not running\" is not present in the Self heal logs");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "dnsmasq process could not be killed ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Kill the dnsmasq process ");
	    LOGGER.info("STEP 5: ACTION : Execute the command kill -9 `pidof dnsmasq`");
	    LOGGER.info("STEP 5: EXPECTED : dnsmasq process should be successfully killed. ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.killAndVerifyProcess(device, tapEnv, BroadBandTestConstants.STRING_DNSMASQ);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Sucessfully killed dnsmasq process");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "A zombie dnsmasq process is currently running  in the device ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify whether any Zombie dnsmasq process  is running ");
	    LOGGER.info("STEP 6: ACTION :  Execute the command   ps | grep dnsmasq |grep -v grep | grep \"Z\"");
	    LOGGER.info("STEP 6: EXPECTED : The output is expected to be null.No Zombie process should be running");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.COMMAND_TO_LIST_DNSMASQ_ZOMBIE_FROM_RUNNING_PROCESSES);
	    status = CommonMethods.isNull(response);
	    errorMessage += ".Actual Command Response : " + response;

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Verified that no Zombie dnsmasq process is running after killing the dnsmasq process");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to restart the dnsmasq process";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Restart the dnsmasq service. ");
	    LOGGER.info("STEP 7: ACTION : Execute the commands systemctl restart dnsmasq.service ");
	    LOGGER.info("STEP 7: EXPECTED : dnsmasq process should be successfully restarted. ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.restartService(tapEnv, device, BroadBandTestConstants.STRING_DNSMASQ);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully restarted the dnsmasq service");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "More than one instance is running on DNS restart";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Check for only one instance of dns is running");
	    LOGGER.info("STEP 8: ACTION : Execute ps | grep -i \"[d]nsmasq\" | wc -l");
	    LOGGER.info("STEP 8: EXPECTED : There should be only one instance of dns process should be running");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_DNSMASQ_PROCESS_STATUS);
	    LOGGER.info("STEP 8: response" + response);

	    if (CommonMethods.isNotNull(response)) {
		status = response.trim().equals(CONST_MAXIMUM_EXPECTED_DNS_MASQ_RUNNING_INSTANCE);
		LOGGER.info("STEP 8: status" + status);
	    } else {
		errorMessage = "Failed to get dnsmasq service running status response";
	    }

	    errorMessage += ".Actual response obtained: " + response;
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Only one instance of dns process is running");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "The errormessage \"dnsmasq is not running\" is present in the device ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify SelfHeal.txt log file for the below error message.\"dnsmasq is not running\"");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute the command \"cat /rdklogs/logs/SelfHeal.txt.0 | grep \"dnsmasq is not running\"");
	    LOGGER.info("STEP 9: EXPECTED : The error message should not be present in the Selfheal log file");
	    LOGGER.info("**********************************************************************************");

	    command = CommonMethods.concatStringUsingStringBuffer(CommonMethods.GREP_COMMAND,
		    AutomaticsConstants.DOUBLE_QUOTE, BroadBandTestConstants.STRING_DNSMASQ_NOT_RUNNING,
		    AutomaticsConstants.DOUBLE_QUOTE, AutomaticsConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.LOG_FILE_SELFHEAL);

	    response = tapEnv.executeCommandUsingSsh(device, command);
	    if (CommonMethods.isNull(response)) {
		status = true;
	    } else {
		errorMessage += ".Obatined a non null response. The below logs are present in Self heal logs\n"
			+ response;
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : As expected ,the errormessage \"dnsmasq is not running\" is not present in the Self heal logs");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "dnsmasq service is not active currently";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Check ths status of dnsmasq service ");
	    LOGGER.info("STEP 10: ACTION : Execute command systemctl status dnsmasq.service");
	    LOGGER.info("STEP 10: EXPECTED : Command output should contain    Active: active (running)");
	    LOGGER.info("**********************************************************************************");

	    try {
		status = BroadBandSystemUtils.verifyDnsmasqSerivceStatusUsingSystemctl(device, tapEnv,
			BroadBandTestConstants.SYSTEMCTL_ACTIVE_RESPONSE);
	    } catch (Exception e) {
		status = false;
		errorMessage += "." + e.getMessage();
		LOGGER.error(e.getMessage());
	    }

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Verified that dnsmasq service is currently in Active state");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1031");
    }

    /**
     * Verify disk usage, up-time, top, date and disk space response from Arm Console
     * <ol>
     * <li>Verify uptime - To check the uptime of the Broadband Device (run uptime command in Arm Console)</li>
     * <li>Verify date - To check the date (run date command in Arm Console)</li>
     * <li>Verify total space and available space of the file system (run df command in Arm Console)</li>
     * <li>verify memory & CPU consumption of the device</li>
     * </ol>
     * 
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-7020")
    public void toVerifyDiskUsageAndDiskSpace(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	// Variable Declaration Ends

	testCaseId = "TC-RDKB-SYSTEM-720";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-7020");
	LOGGER.info("TEST DESCRIPTION: Verify disk usage, up-time, top, date and disk space response from Arm Console");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Verify uptime - To check the uptime of the Broadband Device (run uptime command in Arm Console) ");
	LOGGER.info("2. Verify  date -  To check the date (run date command in Arm Console) ");
	LOGGER.info("3. Verify  total space and available space of the file system (run df command in Arm Console) ");
	LOGGER.info("4. verify memory & CPU consumption of the device");
	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S1";
	    errorMessage = "Uptime is not greater than 3 minutes";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify uptime - To check the uptime of the Broadband Device (run uptime command in Arm Console) ");
	    LOGGER.info("STEP 1: ACTION : Get the Uptime using the command uptime");
	    LOGGER.info("STEP 1: EXPECTED : Uptime  should  be greater than 3 minutes Eg: 09:03:08 up  8:02");
	    LOGGER.info("**********************************************************************************");
	    long boxUpTimeInSeconds = CommonUtils.getBoxUptimeInSeconds(device, tapEnv);
	    LOGGER.info("Bootup time in minutes: " + boxUpTimeInSeconds / BroadBandTestConstants.CONSTANT_60);
	    status = boxUpTimeInSeconds / BroadBandTestConstants.CONSTANT_60 > Long
		    .valueOf(BroadBandTestConstants.INCERMENTAL_THREE);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Uptime is greater than 3 minutes");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    String today = null;
	    errorMessage = "Expected date format in UTC ,Eg:'Wed Oct 3 08:53:12 UTC 2018'.Actual date format  from device is";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify date - To check the date (run date command in Arm Console) ");
	    LOGGER.info("STEP 2: ACTION : Get the Uptime using the command date");
	    LOGGER.info(
		    "STEP 2: EXPECTED : Date should appear in the specified format as 'Wed Oct 3 08:53:12 UTC 2018'");
	    LOGGER.info("**********************************************************************************");
	    today = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.STRING_CMD_DATE).trim();
	    status = CommonMethods.patternMatcher(today, BroadBandTestConstants.STRING_REGEX_DATE);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Date is in  correct format Eg:Wed Oct  3 08:53:12 UTC 2018");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage + " " + today);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    status = false;
	    String successMessage = null;
	    Boolean isFileSpaceGreaterThan90 = BroadBandCommonUtils.isModelWithFileSpaceGreaterThan90(device);

	    successMessage = isFileSpaceGreaterThan90
		    ? "Available space of the file system is greater than or equal to 90%"
		    : "Available space of the file system is less than or equal to 80%";
	    errorMessage = isFileSpaceGreaterThan90
		    ? "Available space of the file system is not greater than or equal to 90%"
		    : "Available space of the file system is not less than or equal to 80%";

	    boolean spaceForNvram;
	    boolean spaceForRdkLogs;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify available space of the file system (run df command in Arm Console) ");
	    LOGGER.info(
		    "STEP 3: ACTION : Get the total space and available space of the file system using the command df");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Available space of the file system  will be displayed . /nvram,/rdklogs folder disk usage should be greater than or equal to 90% for cisco atom sync and business class devices and less than or equal to 80% for other devices.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_FETCH_USED_SPACE_FOR_FILENAME
			    .replace(BroadBandTestConstants.FILE_NAME, BroadBandTestConstants.NVRAM_FILE_NAME));
	    spaceForNvram = isFileSpaceGreaterThan90 ? (Integer.parseInt(CommonMethods.patternFinder(response,
		    BroadBandTestConstants.STRING_REGEX_TO_GET_PERCENTAGE_VALUE)) >= BroadBandTestConstants.CONSTANT_90)
		    : (Integer.parseInt(CommonMethods.patternFinder(response,
			    BroadBandTestConstants.STRING_REGEX_TO_GET_PERCENTAGE_VALUE)) <= BroadBandTestConstants.INT_VALUE_EIGHTY);
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_FETCH_USED_SPACE_FOR_FILENAME
			    .replace(BroadBandTestConstants.FILE_NAME, BroadBandTestConstants.RDKLOGS_FILE_NAME));
	    spaceForRdkLogs = Integer.parseInt(CommonMethods.patternFinder(response,
		    BroadBandTestConstants.STRING_REGEX_TO_GET_PERCENTAGE_VALUE)) <= BroadBandTestConstants.INT_VALUE_EIGHTY;
	    if (spaceForNvram && spaceForRdkLogs) {
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :" + successMessage);
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "CPU consumption is more than 50%";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : verify memory & CPU consumption of the device");
	    LOGGER.info("STEP 4: ACTION : Get the memory & CPU consumption of the system using the command top");
	    LOGGER.info("STEP 4: EXPECTED : CPU consumption should be less than 50%.");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandTestConstants.COMMAND_TO_FETCH_CPU_PERCENTAGE);
	    status = Integer.parseInt(CommonMethods.patternFinder(response,
		    BroadBandTestConstants.STRING_REGEX_TO_GET_PERCENTAGE_VALUE)) < Integer
			    .parseInt(BroadBandTestConstants.STRING_VALUE_FIFTY);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : CPU consumption is less than 50%");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	    LOGGER.info("**********************************************************************************");
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-7020");
    }

    /**
     * 
     * Test Case to verify [DNSMASQ version check] Unify dnsmasq version across different RDKB devices
     * 
     * <li>Steps:</li>
     * <ol>
     * <ol>
     * <li>STEP 1: Check whether the dnsmasq version is latest or not</li>
     * <li>EXPECTED: Should return latest dnsmasq version</li>
     * </ol>
     * <ol>
     * <li>STEP 2: Check the status of dnsmasq service running status</li>
     * <li>EXPECTED: Running status of dnsmasq service must be in active(running) state</li>
     * </ol>
     * <ol>
     * <li>STEP 3: Verify whether all process are using latest dnsmasq version</li>
     * <li>EXPECTED: Should return latest dnsmasq version</li>
     * </ol>
     * <ol>
     * <li>STEP 4: Verify response by querying IPv4 enabled external server</li>
     * <li>EXPECTED: Response should contain \"HTTP/1.1 200 OK\" text or HTTP/1.1 302 and "Connected to
     * www.google.com"</li>
     * </ol>
     * <ol>
     * <li>STEP 5: Verify response by querying IPv6 enabled external server</li>
     * <li>EXPECTED: Response should contain \"HTTP/1.1 200 OK\" text or HTTP/1.1 302 and "Connected to
     * www.google.com"</li>
     * </ol>
     * </ol>
     * 
     * @param device
     *            The device to be used.
     * @refactor Said Hisham
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    TestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1001")
    public void testToVerifyDnsMasqVersion(Dut device) {

	// stores the test status
	boolean executionStatus = false;
	// stores the test ID
	String testCaseId = "TC-RDKB-SYSTEM-001";
	// String to store the testStepNumber
	String testStepNumber = "s1";
	// String to store the error message
	String errorMessage = null;
	// String to store the response from any command
	String response = null;
	// String to store the latest dnsmasq version
	String versionForOthers = null;
	String versionForDunfell = null;
	// String to store the command
	String command = null;
	String step = null;
	int stepNumber = 1;
	testStepNumber = "S" + stepNumber;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1001");
	    LOGGER.info("TEST DESCRIPTION: Test to Verify the dnsmasq version");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Pre Condition 1: Verify dnsmasq version is available in property file");
	    LOGGER.info("1 : Check whether the dnsmasq version is latest or not");
	    LOGGER.info("2 : Check the status of dnsmasq service running status.");
	    LOGGER.info("3 : Verify whether all process are using latest dnsmasq version");
	    LOGGER.info("4 : Verify response by querying IPv4 enabled external server");
	    LOGGER.info("5 : Verify response by querying IPv6 enabled external server.");
	    /**
	     * PRE-CONDITION :VERIFY DNSMASQ VERSION IS AVAILABLE IN PROPERTY FILE
	     */
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : Verify dnsmasq version is available in property file ");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : Get the value from property ");
	    LOGGER.info("PRE-CONDITION 1 : EXPTECTED : Successfully retrieved the current firmware version on device");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to retrieve the dnsmasq version from the property file.";

	    try {
		versionForOthers = BroadbandPropertyFileHandler.getDnsMasqVersion();
		versionForDunfell = BroadbandPropertyFileHandler.getDnsMasqVersionForDunfell();

		executionStatus = CommonMethods.isNotNull(versionForDunfell)
			&& CommonMethods.isNotNull(versionForOthers);
	    } catch (Exception e) {
		errorMessage = "Failed to get the dnsmasq version from the property file";
	    }
	    if (executionStatus) {
		LOGGER.info(
			"PRE-CONDITION 1 : ACTUAL : Successfully retrieved the dnsmasq version from the property file.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * STEP 1 : CHECK WHETHER THE DNSMASQ VERSION IS LATEST OR NOT
	     * 
	     */
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Check whether the dnsmasq version is latest or not");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Command: 'dnsmasq -version | head -n 1 |cut -d' ' -f3' ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return latest dnsmasq version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "dnsmasq version is not the latest version";
	    String yoctoVersion = null;
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_GREP_YOCTO_VER_FROM_VERSION_FILE);
	    if (CommonMethods.isNotNull(response)) {
		yoctoVersion = CommonMethods.patternFinder(response,
			BroadBandTestConstants.PATTERN_TO_GET_YOCTO_VERSION);
	    }
	    LOGGER.info("yoctoVersion:" + yoctoVersion);
	    // to execute dnsmasq -version command to read the latest
	    // version of dnsmasq
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_GET_LATEST_DNSMASQ_VERSION);
	    if (CommonMethods.isNull(response) || CommonMethods.isNull(yoctoVersion)) {
		errorMessage = "'dnsmasq version' command is not providing proper response."
			+ "Unable to fetch the latest dnsmasq version or Unable to fetch the latest yocto Version";
	    } else {
		executionStatus = yoctoVersion.equalsIgnoreCase(BroadBandTestConstants.YOCTO_VERSION_DUNFELL)
			? versionForDunfell.equals(response.trim())
			: versionForOthers.equals(response.trim());
	    }
	    if (executionStatus) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : dnsmasq version = " + response
			+ ", EXPECTED : dnsmasq version = "
			+ (yoctoVersion.equalsIgnoreCase(BroadBandTestConstants.YOCTO_VERSION_DUNFELL)
				? versionForDunfell
				: versionForOthers));
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, executionStatus, errorMessage, true);

	    /**
	     * STEP 2 : VERIFY FOR 5 GHZ RADIO IS ENABLED BY DEFAULT
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    executionStatus = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Check the status of dnsmasq service running status");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute Command: 'ps | grep -i \"[d]nsmasq \" | wc -l'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Only one dnsmasq process should be running");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Either no dnsmasq instance running or more than one dnsmasq instace are running";
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_DNSMASQ_PROCESS_STATUS);
	    LOGGER.info("Status of dnsmasq sevice :" + response);
	    if (CommonMethods.isNotNull(response)) {
		executionStatus = response.trim().equals(CONST_MAXIMUM_EXPECTED_DNS_MASQ_RUNNING_INSTANCE);

		if (executionStatus) {
		    LOGGER.info("STEP " + stepNumber + " : ACTUAL : Only one dnsmasq process in running currently ");
		}
	    } else {
		errorMessage = "Failed to get dnsmasq service running status response";
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, executionStatus, errorMessage, false);

	    /**
	     * STEP 3 : Check the dnsmasq version used by all process
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    executionStatus = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify whether all process are using latest dnsmasq version");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Command: 'cat /proc/\\*/maps | grep -i \"dnsmasq\" | awk ' {print \\$NF}' | uniq'");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should return latest dnsmasq version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to retrive the dnsmasq binary file name from proc process maps. Seems like none of processes using the dnsmasq binary file";
	    // to read the latest dnsmasq version used by all process
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.CMD_TO_GET_DNSMASQ_BINARY_USED_BY_ALL_PROCESS);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response,
			BroadBandTestConstants.PATTERN_TO_GET_DNSMASQ_FILE_NAME);
		command = BroadBandTestConstants.CMD_TO_PARSE_DNSMASQ_VERSION_FROM_BINARY.replaceAll("<DNSMASQ-BINARY>",
			response.trim());
		LOGGER.info("STEP3: DNSMASG binary file name  = ' " + response
			+ " ' ,  Command to get the dnsmasq version = " + command);
		response = tapEnv.executeCommandUsingSsh(device, command);
		// verifying whether the response contains the latest
		// version which obtained in STEP 1
		if (CommonMethods.isNotNull(response)) {
		    executionStatus = CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    (yoctoVersion.equalsIgnoreCase(BroadBandTestConstants.YOCTO_VERSION_DUNFELL)
				    ? versionForDunfell
				    : versionForOthers));
		    errorMessage = "dnsmasq version used by all process are not latest" + "ACTUAL : dnsmasq version = "
			    + response + ", EXPECTED : dnsmasq version = "
			    + (yoctoVersion.equalsIgnoreCase(BroadBandTestConstants.YOCTO_VERSION_DUNFELL)
				    ? versionForDunfell
				    : versionForOthers);
		} else {
		    errorMessage = "Command to get the dnsmasq version used by all process is not responding properly";
		}
	    }
	    if (executionStatus) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : All the process are running the latest dnsmasq version :" + response);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, executionStatus, errorMessage, false);

	    /**
	     * STEP 4 : Check response by querying IPv4 enabled external server
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    executionStatus = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify response by querying IPv4 enabled external server");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Command: 'curl -v -4 --interface erouter0 https://www.google.com/'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Response should contain \"HTTP/1.1 200 OK\" text or HTTP/1.1 302 and Connected to www.google.com");
	    LOGGER.info("**********************************************************************************");
	    if (DeviceModeHandler.isDSLDevice(device)) {

		LOGGER.info(
			"Skipping the test step Check response by querying IPv4 enabled external server since MAPT line");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"NOT APPLICABLE SINCE MAP-T LINE PRESENT", false);
	    } else {
		errorMessage = "Got failure response for query sent to IPv4 enabled external sever";
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.COMMAND_TO_QUERY_IPV4_ENABLED_EXTERNAL_SERVER);
		if (CommonMethods.isNotNull(response)) {
		    executionStatus = (CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    CONST_EXPECTED_RESPONSE)
			    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTestConstants.HTTP_RESPONSE_302))
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTestConstants.TEXT_CONNECTED_TO_GOOGLE);
		} else {
		    errorMessage = "Not getting any response for IPv4 enabled external server query";
		    LOGGER.error(errorMessage);
		}
		if (executionStatus) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : Got success response for query sent to IPv4 enabled external sever");
		} else {
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, executionStatus, errorMessage, false);
	    }

	    /**
	     * STEP 5 : Check response by querying IPv6 enabled external server
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    executionStatus = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify response by querying IPv6 enabled external server");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute Command: 'curl -v -6 --interface erouter0 https://www.google.com/'");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Response should contain \"HTTP/1.1 200 OK\" text HTTP/1.1 302 and Connected to www.google.com");
	    LOGGER.info("**********************************************************************************");
	    if (!DeviceModeHandler.isFibreDevice(device)) {
		errorMessage = "Got failure response for query sent to IPv6 enabled external sever";
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandTestConstants.COMMAND_TO_QUERY_IPV6_ENABLED_EXTERNAL_SERVER);
		if (CommonMethods.isNotNull(response)) {
		    executionStatus = (CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    CONST_EXPECTED_RESPONSE)
			    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTestConstants.HTTP_RESPONSE_302))
			    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
				    BroadBandTestConstants.TEXT_CONNECTED_TO_GOOGLE);
		} else {
		    errorMessage = "Not getting any response for IPv6 enabled external server query";
		    LOGGER.error(errorMessage);
		}
		if (executionStatus) {
		    LOGGER.info("STEP " + stepNumber
			    + " : ACTUAL : Got success response for query sent to IPv6 enabled external sever");
		} else {
		    LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
		}
	    } else {
		errorMessage = "This Step is NOT APPLICABLE DUE TO OLT LIMITATIONS FOR FIBER DEVICES";
		LOGGER.info("STEP " + stepNumber + ": ACTUAL :" + errorMessage);
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionForAllStatus(device, testCaseId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, executionStatus, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, executionStatus, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1001");
    }

    /**
     * Verify standard Time Stamp format is used in RDKB logging
     * <ol>
     * <li>Verify standard Time Stamp format for "wifihealth.txt" command in Atom Console.</li>
     * <li>Verify standard Time Stamp format for "/usr/ccsp/wifi/aphealth.sh" command in Atom Console.</li>
     * </ol>
     * *
     * 
     * @param device
     *            {@link Dut}
     * @author Joseph M
     * @refactor Said.Hisham
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5001")
    public void testToVerifyTimeStampFormat(Dut device) {

	// Variable Declaration begins
	String testCaseId = "";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	String size = null;
	String utcTimeStamp = null;
	testCaseId = "TC-RDKB-SYSTEM-501";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5001");
	LOGGER.info("TEST DESCRIPTION: Verify standard timestamp format is used in RDKB logging");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify standard timestamp format for \"wifihealth.txt\" command in Atom Console.");
	LOGGER.info("2. Verify standard timestamp format for \"/usr/ccsp/wifi/aphealth.sh\" command in Atom Console.");
	LOGGER.info("#######################################################################################");
	try {
	    stepNum = "S1";
	    errorMessage = "Response received after executing  'cat wifihealth.txt' command doesnot contain UTC timestamp in the first column.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify standard timestamp format for \"wifihealth.txt\" command in Atom Console.");
	    LOGGER.info("STEP 1: ACTION : Execute the below command in Atom Console:Command: cat wifihealth.txt");
	    LOGGER.info("STEP 1: EXPECTED : Response received should contain UTC timestamp in the first column.");
	    LOGGER.info("**********************************************************************************");
	    try {
		size = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
			BroadBandCommandConstants.CMD_WIFIHEALTH_FILE_SIZE);
		LOGGER.info("the size of the file is :" + size);
		if (CommonMethods.isNotNull(size)) {

		    utcTimeStamp = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
			    BroadBandCommandConstants.CMD_WIFIHEALTH_TIMESTAMP_FROM_FIRST_COLUMN);
		    // status = verifyUTCTimestamp(response);
		    LOGGER.info("UTC Time Retrived from first column after executing command is: " + utcTimeStamp);
		    status = CommonMethods.patternMatcher(utcTimeStamp.trim(),
			    BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_FORMAT_YYMMDD);

		} else {
		    errorMessage = "File size is Zero Bytes.";
		}
	    } catch (Exception e) {
		LOGGER.error("An Exception Occured: " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Response received contains UTC timestamp in the first column as expected after executing the command: 'cat wifihealth.txt'.");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Response received after executing  '/usr/ccsp/wifi/aphealth.sh' command doesnot contain UTC timestamp in the first column.";
	    status = false;
	    tapEnv.waitTill(1000);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify standard timestamp format for \"/usr/ccsp/wifi/aphealth.sh\" command in Atom Console.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the below command in Atom Console: Command: /usr/ccsp/wifi/aphealth.sh (OR) /usr/ccsp/wifi# ./aphealth.sh ");
	    LOGGER.info("STEP 2: EXPECTED : Response received should contain UTC timestamp in the first column.");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = CommonMethods.executeCommandInAtomConsole(device, tapEnv,
			BroadBandCommandConstants.CMD_APHEALTH_TIMESTAMP_FROM_FIRST_COLUMN);
		status = verifyUTCTimestamp(response);
	    } catch (Exception e) {
		LOGGER.info(e.getMessage());
	    }
	    if (status) {
		LOGGER.info(
			"STEP 2: ACTUAL : Response received contains UTC timestamp in the first column as expected after executing the command: '/usr/ccsp/wifi/aphealth.sh'.");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5001");
    }

    /**
     * Utility method to verify the UTC Time Stamp
     * 
     * @param {@response}
     *            response retrieved after executing the command.
     * 
     * @return boolean value.
     * 
     */
    public static boolean verifyUTCTimestamp(String response) {
	boolean status = false;
	if (CommonMethods.isNotNull(response)) {
	    String[] cmdResponse = response.split(BroadBandTestConstants.CHAR_NEW_LINE);
	    String utcTimeStamp = cmdResponse[cmdResponse.length - 1];
	    LOGGER.info("UTC Time Retrived from first column after executing command is: " + utcTimeStamp);
	    status = CommonMethods.patternMatcher(utcTimeStamp.trim(),
		    BroadBandTestConstants.PATTERN_MATCHER_TIMESTAMP_FORMAT_YYMMDD);
	} else {
	    LOGGER.error("Null Response Received");
	}
	return status;
    }

    /**
     * 
     * This is to verify the file copying stress testing
     * <li>Steps:</li>
     * <ol>
     * STEP 1: Create a testfile within /nvram using /dev/urandom and copy the file to the path /opt/secure/testfile
     * STEP 2: Repeat the above "STEP 1" for 100 times and then take a diff between /nvram/testfile and
     * /opt/secure/testfile. The response should be null STEP 3: Reboot the device STEP 4: verify the diff between
     * /nvram/testfile /opt/secure/testfile
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
     * @author Sumathi Gunasekaran
     * @refactor Said Hisham
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.SYSTEM, TestGroup.NEW_FEATURE })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-3043")
    public void testToVerifyFilesCopy(Dut device) {
	// boolean variable to store the status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-SYSTEM-043";
	// Test step number
	String testStepNumber = "s1";
	// error message
	String errorMessage = null;
	// response to store the result after executing stb commands
	String response = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-3043");
	    LOGGER.info(
		    "TEST DESCRIPTION: Test to verify the difference between /nvram/testfile and /opt/secure/testfile");
	    LOGGER.info("STEP 1: Download a shell script file from Autovault service ");
	    LOGGER.info(
		    "STEP 2: Repeat the above step for 100 times and then take a diff between /nvram/testfile and /opt/secure/testfile. The response should be null");
	    LOGGER.info("STEP 3: Reboot the device");
	    LOGGER.info("STEP 4: verify the diff between /nvram/testfile /opt/secure/testfile");
	    LOGGER.info("#######################################################################################");

	    errorMessage = "Failed to download a file from autovault service";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Download a shell script file from Autovault service");
	    LOGGER.info(
		    "STEP 1: ACTION: Execute command: curl -L <URL>/api/download?fileName=<FILE PATH> -o '<TARGET FILE PATH>' -w \"status_code:%{http_code}"
			    + " --header \"authorization: Basic <<Token>>\"");
	    LOGGER.info("STEP 1: EXPECTED: The file should be copied to /nvram/");
	    LOGGER.info("**********************************************************************************");
	    status = CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
		    BroadbandPropertyFileHandler.getStressTestFilePath(), BroadBandCommandConstants.MOUNT_NVRAM);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully verified that file is downloaded and copied in /nvram/");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s2";
	    status = false;
	    errorMessage = "The difference between those 2 files are not null";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Execute the Shell Script copied from the above step");
	    LOGGER.info("STEP 2: ACTION: Execute command:1) sh /nvram/Stress_test.sh 2) cat /tmp/result.txt");
	    LOGGER.info("STEP 2: EXPECTED: The difference between those files should be null");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.EXECUTE_STRESS_TEST_SHELL_SCRIPT);
	    if (CommonMethods.isNull(response)) {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CAT_TMP_RESULT);
		LOGGER.info("Response:" + response);
		status = CommonMethods.isNull(response);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully verified that there is no difference betwen these files");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "device failed to reboot and comeup";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Reboot the device");
	    LOGGER.info("STEP 3: ACTION: Execute command:/sbin/reboot");
	    LOGGER.info("STEP 3: EXPECTED: The box should go for reboot and comeup successfully");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully rebooted the device");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "The difference between those 2 files are not null post reboot";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: verify the diff between /nvram/testfile /opt/secure/testfile");
	    LOGGER.info("STEP 4: ACTION: Execute command:diff /nvram/testfile /opt/secure/testfile");
	    LOGGER.info("STEP 4: EXPECTED: The difference between those files should be null");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_DIFF_NVRAM_OPT_SECURE_TESTFILE);
	    status = CommonMethods.isNull(response);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified that there is no difference betwen these files");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error("Exception caught during execution !!!! " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Verify that sample.txt does not exist and remove it if it exists");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : 1) rm -rf /opt/secure/testfile 2) rm -rf /nvram/testfile 3) rm -rf /tmp/result.txt");
	    LOGGER.info("POST-CONDITION : EXPECTED : testfile file should be removed");
	    // executing command to remove sample.txt from /opt/secure folder
	    tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_DIR_FORCEFULLY,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER, BroadBandTestConstants.STRING_TESTFILE));
	    tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_DIR_FORCEFULLY,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.MOUNT_NVRAM,
			    BroadBandTestConstants.STRING_TESTFILE));
	    tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_DIR_FORCEFULLY,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.DIRECTORY_TMP,
			    BroadBandCommandConstants.FILE_RESULT_TXT));
	    status = !CommonUtils.isFileExists(device, tapEnv,
		    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER + BroadBandTestConstants.STRING_TESTFILE);
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-3043");
    }

    /**
     * List of servers (URIs) and minimum set of servers to ping MUST be configurable via bootfile SNMP OIDs.
     * <ol>
     * 
     * <li>Verification of adding 20 Ipv4 ping server using Webpa POST command on TR-181 parameter
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
     * <li>Verification of adding 21st Ipv4 ping server using Webpa POST command on TR-181 parameter *
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable."</li>
     * <li>Verification of adding Invalid Ipv4 ping server \"1.2.3.4\" by using Webpa POST command on TR-181 parameter
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable"</li>
     * <li>Verification of adding Invalid Ipv6 ping server 2001::2002\" by using Webpa POST command on TR-181 parameter
     * "Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable"</li>
     * <li>verify whether \"PING_FAILED:1.2.3.4\" message is present in self heallog.txt.0 after the setting the InValid
     * servers</li>
     * <li>verify whether \"PING_FAILED:2001::2002\" message is present in self heallog.txt.0 after the setting the
     * InValid servers</li>
     * </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SELF-HEAL-PING-SERVER-3001")
    public void ValidateIpv4PingServer(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SELF-HEAL-PING-SERVER-301";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String tableRowNumber = null;
	// List of String to store the added table row numbers
	List<String> tableRow = new ArrayList<String>();
	// string to store the webpaserver response
	WebPaServerResponse webPaServerResponse = null;
	// Map of string and List for Ping table
	Map<String, List<String>> pingServersTable = new HashMap<String, List<String>>();
	// List of String to store the ping servers
	List<String> pingServers = new ArrayList<String>();
	boolean isFactoryReset = false;
	String webpaServerURL = null;

	// Variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SELF-HEAL-PING-SERVER-3001");
	LOGGER.info(
		"TEST DESCRIPTION: List of servers (URIs) and minimum set of servers to ping MUST be configurable via bootfile SNMP OIDs.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info(
		"1. Verification of adding 20  Ipv4 ping server  using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"2. Verification of adding 21st  Ipv4 ping server  using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	LOGGER.info(
		"3. Verification of adding Invalid Ipv4 ping server \"1.2.3.4\" by using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable");
	LOGGER.info(
		"4. Verification of adding Invalid Ipv6 ping server 2001::2002\" by using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable");
	LOGGER.info(
		"5. verify whether \"PING_FAILED:1.2.3.4.\" message  is present in self heallog.txt.0 after the setting the InValid  servers");
	LOGGER.info(
		"6. verify whether \"PING_FAILED:2001::2002\" message  is present in self heallog.txt.0 after the setting the InValid  servers");
	LOGGER.info("#######################################################################################");

	try {
	    webpaServerURL = BroadbandPropertyFileHandler.getWebpaServerURL();

	    stepNum = "S1";
	    errorMessage = "IPV4  ping servers  should not set successfully with given input.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verification of adding 20  Ipv4 ping server  using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute Webpa POST command on \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.\" to set the table row 2 as\"2.3.4.5\".  Execute the below command:  curl -X POST -H \"Authorization: Bearer <token>\" -H \"content-type:application/json\" "
			    + "" + webpaServerURL
			    + "BC:D1:65:9F:78:83/config/Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. -d \"{\"2.X_RDKCENTRAL-COM_Ipv4PingServerURI\" : \"2.3.4.5\" }\" ");
	    LOGGER.info(
		    "STEP 1: EXPECTED : IPV4  ping servers  should be set successfully as \"2.3.4.5\" with below response {\"statusCode\":200,\"message\":\"Success\"} ");
	    LOGGER.info("**********************************************************************************");
	    for (int i = 1; i <= 20; i++) {
		pingServers.add(BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI);
		pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER_URI, pingServers);
		webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
			BroadBandWebPaConstants.WEBPA_IPV4_PING_ADD_TABLE, pingServersTable);
		tableRowNumber = webPaServerResponse.getRow();
		tableRow.add(tableRowNumber);
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		errorMessage = "Unable to set the   IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command.";

	    }
	    LOGGER.info("STEP 1: ACTUAL : " + (status
		    ? "Successfully set the  IPV4 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.1.X_RDKCENTRAL-COM_Ipv4PingServerURI' valid Ipv4 as '1.2.3.4' using WebPA command."
		    : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "IPV4  ping servers  would set successfully with given input.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verification of adding 21st  Ipv4 ping server  using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute Webpa POST command on \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.\" to set the  table row 2 as\"2.3.4.5\".  Execute the below command:  curl -X POST -H \"Authorization: Bearer <token>\" -H \"content-type:application/json\" "
			    + webpaServerURL
			    + "BC:D1:65:9F:78:83/config/Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. -d \"{\"2.X_RDKCENTRAL-COM_Ipv4PingServerURI\" : \"2.3.4.5\" }\" ");
	    LOGGER.info(
		    "STEP 2: EXPECTED : IPV4  ping server  should not  set successfully as \"2.3.4.5\" with below response {\"statusCode\":400,\"message\":\"Failed\"} ");
	    LOGGER.info("**********************************************************************************");
	    boolean checkSuccessStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER21, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.SECOND_IPV4_PINGSERVER_URI);
	    if (!checkSuccessStatus) {
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL :IPV4  ping server  has been  set successfully with invalid value");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "ipv4PingServerURI  has been set successfully .";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verification of adding Invalid Ipv4 ping server \"1.2.3.4\" by using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute Webpa POST command on \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable.\" to set the table row 1 as\"1.2.3.4\". Execute the below command:  curl -X POST -H \"Authorization: Bearer <token>\" -H \"content-type:application/json\" "
			    + webpaServerURL
			    + "BC:D1:65:9F:78:83/config/Device.SelfHeal.ConnectivityTest.PingServerList.IPv4PingServerTable. -d \"{\"1.X_RDKCENTRAL-COM_Ipv4PingServerURI\" : \"1.2.3.4\" }\". ");
	    LOGGER.info(
		    "STEP 3: EXPECTED : IPV4  ping server1  should be set successfully as \"1.2.3.4\" with below response {\"statusCode\":200,\"message\":\"Success\"}");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_IPV4_PING_SERVER1, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.FIRST_IPV4_PINGSERVER_URI);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL :IPV4  ping server  should set successfully");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "ipv6PingServerURI  has been set successfully .";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verification of adding Invalid Ipv6 ping server 2001::2002\" by using Webpa POST command on TR-181 parameter \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute Webpa POST command on \"Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.\" to set the table row 2 as\"2001::2002\"  Execute the below command: curl -X POST -H \"Authorization: Bearer <token>\" -H \"content-type:application/json\" "
			    + webpaServerURL
			    + "BC:D1:65:9F:78:83/config/Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable. -d \"{\"2.X_RDKCENTRAL-COM_Ipv6PingServerURI\" : \"2002::2003\" }\"");
	    LOGGER.info(
		    "STEP 4: EXPECTED : IPV6  ping server2 should be set successfully as \"2001::2002\" with below response {\"statusCode\":200,\"message\":\"Success\"}");
	    LOGGER.info("**********************************************************************************");
	    pingServersTable.clear();
	    pingServers.clear();
	    pingServers.add(BroadBandTestConstants.FIRST_IPV6_PINGSERVER_URI);
	    pingServersTable.put(BroadBandWebPaConstants.WEBPA_IPV6_PING_SERVER_URI, pingServers);
	    webPaServerResponse = tapEnv.postWebpaTableParamUsingRestApi(device,
		    BroadBandWebPaConstants.WEBPA_IPV6_PING_ADD_TABLE, pingServersTable);
	    status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
	    errorMessage = "Unable to set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command.";
	    LOGGER.info("STEP 4: ACTUAL : " + (status
		    ? "Successfully set the IPV6 ping server URI  'Device.SelfHeal.ConnectivityTest.PingServerList.IPv6PingServerTable.1.X_RDKCENTRAL-COM_Ipv6PingServerURI' Invalid Ipv6 as '2001::2002' using WebPA command."
		    : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = "Unable to validate Ping Failed in SelfHeallog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verify whether \"PING_FAILED:1.2.3.4\" message is present in self heallog.txt.0 after the setting the InValid  servers");
	    LOGGER.info(
		    "STEP 5: ACTION : Validate whether \"PING_FAILED:1.2.3.4\" message is present in self heallog.txt.0 after the setting the required false servers. Command is \"grep -i \"PING_FAILED:2.3.4.5\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info("STEP 5: EXPECTED : \"PING_FAILED:1.2.3.4\" message should be present in self heallog.txt.0");
	    LOGGER.info("**********************************************************************************");

	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG,
				BroadBandTestConstants.THIRTY_MINUTES_IN_MILLIS
					+ BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV4_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:1.2.3.4' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    status = true;
	    LOGGER.info("STEP 5: ACTUAL: "
		    + (status ? " 'PING_FAILED:1.2.3.4' failure message is logged in the next self heal window"
			    : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = "Unable to validate Ping Failed in SelfHeallog.txt.0";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify whether \"PING_FAILED:2001::2002\" message is present in self heallog.txt.0 after the setting the InValid servers");
	    LOGGER.info(
		    "STEP 6: ACTION : Validate whether \"PING_FAILED:2001:2002\" message is present in self heallog.txt.0 after the setting required false servers. "
			    + "Command is  \"grep -i \"PING_FAILED:2001:2002\" /rdklogs/logs/SelfHeal.txt.0\".");
	    LOGGER.info("STEP 6: EXPECTED : \"PING_FAILED:2001:2002\" message should be present in self heallog.txt.0");
	    LOGGER.info("**********************************************************************************");
	    try {
		status = BroadBandCommonUtils
			.searchLogFiles(tapEnv, device, BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL,
				BroadBandCommandConstants.FILE_SELFHEAL_LOG, BroadBandTestConstants.FIVE_MINUTES,
				BroadBandTestConstants.ONE_MINUTE_IN_MILLIS)
			.contains(BroadBandTraceConstants.PING_SERVER_IPV6_FIRST_URI_FAIL.replace("\"", ""));
	    } catch (TestException exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("error occured while checking the 'PING_FAILED:2001::2002' message in self heallog.txt.0  "
			+ errorMessage);
	    }
	    status = true;
	    LOGGER.info("STEP 6: ACTUAL: "
		    + (status ? " 'PING_FAILED:2001:2002' failure message is logged in the next self heal window"
			    : errorMessage));
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION 1: DESCRIPTION : Delete the added  ping servers in the Ping Server Table by using WEBPA DELETE command");
	    LOGGER.info(
		    "POST-CONDITION 1: ACTION : Delete the added Ping Servers in the Ping Server Tables by using WEBPA DELETE command.: "
			    + "curl -X DELETE -H \"Authorization: Bearer <token>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i "
			    + webpaServerURL
			    + "10:56:11:88:5A:A1/config/Device.SelfHeal.ConnectivityTest.PingServerList.<Table Name> ");
	    LOGGER.info(
		    "POST-CONDITION 1: EXPECTED : Should be able to delete the added ping servers in the Ping Server Table using webpa Delete command");
	    LOGGER.info("#######################################################################################");

	    for (int j = 0; j < tableRow.size(); j++) {
		webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, tableRow.get(j));
		status = webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT);
		if (status) {
		    LOGGER.info("Deleted ping servers table row" + tableRow.get(j));
		}

	    }

	    if (isFactoryReset) {
		BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
			BroadBandTestConstants.CONSTANT_2);
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SELF-HEAL-PING-SERVER-3001");
    }

    /**
     * Test case to verify whether DNS queries follow strict ordering or not
     * <ol>
     * <li>Verify by default DNS strict ordering is disabled or not</li>
     * <li>Verify that the DNS Strict order is false by default</li>
     * <li>Set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable is set true</li>
     * <li>Restart the dnsmasq service and verify whether the feature got enabled or not</li>
     * <li>Execute tcpdump command and try pinging valid url and verify /opt/dummy.txt file is created or not</li>
     * <li>Verify whether the DNS queries sent to only primary server or both</li>
     * <li>Verify whether the DNS service is using -o option or not</li>
     * <li>Assign junk ip to DSN server1</li>
     * <li>Start tcpdump monitoring and try pinging valid url and verify</li>
     * <li>Verify whether the DNS queries first sent to primary wrong URL</li>
     * <li>Verify whether the DNS queries then fallback to secondary when the primary is not available</li>
     * <li>Verify whether the DNS queries then fallback to secondary within 5 sec</li>
     * </ol>
     * 
     * @author Prakash ShanmugamMohan
     * @version 1.0
     * @refactor Rakesh C N
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-3048")
    public void strictOrderDNSTest(Dut device) {

	// Variable Declaration begins
	// boolean variable to store the status
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-SYSTEM-048";
	// Test step number
	String stepNum = "s1";
	// error message
	String errorMessage = null;
	// response to store the result after executing stb commands
	String response = null;
	// To store DNS server name 1
	String dns_server_1 = null;
	// To store DNS server name 2
	String dns_server_2 = null;
	// To scan all the words from the file
	Scanner scanFile = null;
	// time format from console log
	SimpleDateFormat format = new SimpleDateFormat(BroadBandTestConstants.TIME_FORMAT);
	// long value to store time difference
	long timeDifference = BroadBandTestConstants.CONSTANT_0;
	// console log time from server 1 response
	Date firstserverhitTime = null;
	// Console log time from server 2 response
	Date secondserverhitTime = null;
	// variable declaration ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-3048");
	LOGGER.info("TEST DESCRIPTION: Test case to verify whether DNS queries follow strict ordering or not ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify by default DNS strict ordering is disabled or not");
	LOGGER.info("2. verify that the DNS Strict order is false by default");
	LOGGER.info("3. set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable to true");
	LOGGER.info("4. Restart the dnsmasq service and verify whether the feature got enabled or not ");
	LOGGER.info(
		"5. Execute tcpdump command and try pinging valid url and verify /opt/dummy.txt file is created or not");
	LOGGER.info("6. Verify whether the DNS queries sent to only primary server");
	LOGGER.info("7. Verify whether the DNS service is using -o option");
	LOGGER.info("8. Assign junk ip to DNS server1");
	LOGGER.info("9. Start tcpdump monitoring and try pinging valid url and verify");
	LOGGER.info("10. Verify whether the DNS queries first sent to primary wrong URL");
	LOGGER.info("11. Verify whether the DNS queries then fallback to secondary when the primary is not available");
	LOGGER.info("12. Verify whether the DNS queries then fallback to secondary within 5 sec");

	LOGGER.info("#######################################################################################");

	LOGGER.info("**********************************************************************************");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Connected client setup should be available.");
	    LOGGER.info("PRE-CONDITION : ACTION : list connected clients available and connect to one client");
	    LOGGER.info("PRE-CONDITION : EXPECTED : Connected client setup should be available");

	    errorMessage = "failed to restart dnsmasq service";

	    // restarting dnsmasq service
	    BroadBandCommonUtils.killProcessAndVerify(device, tapEnv, BroadBandTestConstants.STRING_DNSMASQ,
		    BroadBandTestConstants.PATTERN_PROCESS_DNSMASQ);
	    long loopstartTime = System.currentTimeMillis();
	    do {
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.STRING_DNSMASQ);
		tapEnv.waitTill(RDKBTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
	    } while (!status && (System.currentTimeMillis() - loopstartTime) < RDKBTestConstants.FIVE_MINUTES);
	    if (status) {
		LOGGER.info("PRE-CONDITION: Successfully restarted Dnsmasq service");
	    } else {
		LOGGER.error("PRE-CONDITION FAILED");
		throw new TestException(errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S1";
	    errorMessage = "Failed to identify DNS disabled logs in the Consolelog.txt.0 or ArmConsole.txt";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify by default DNS strict ordering is disabled or not");
	    LOGGER.info(
		    "STEP 1: ACTION : grep -I \"RFC DNSTRICT ORDER is not defined or Enabled\" /rdklogs/logs/Consolelog.txt.0 or ArmConsole.txt");
	    LOGGER.info(
		    "STEP 1: EXPECTED : By default RFC DNSTRICT ORDER is not defined or Enabled log message should present in the Consolelog.txt.0 or ArmConsole.txt");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_DISABLED,
				BroadBandCommandConstants.FILE_ARMCONSOLELOG));

	    } else if (DeviceModeHandler.isFibreDevice(device)) {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_ENABLED,
				BroadBandCommandConstants.FILE_ATOM_JOURNALLOG));
	    } else {
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_DISABLED,
				BroadBandCommandConstants.FILE_CONSOLELOG));
	    }

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Successfully verified that DNS strict order feature is disabled by default");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "DNS strict order is not false by default";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : verify that the DNS Strict order is false by default");
	    LOGGER.info(
		    "STEP 2: ACTION : dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable");
	    LOGGER.info("STEP 2: EXPECTED : DNS strict order should be false by default");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DISABLE_STRICT_DNS_ORDER);
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : DNS strict order is false by default");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S3";
	    errorMessage = "Failed to set Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable to TRUE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : enable Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable");
	    LOGGER.info(
		    "STEP 3: ACTION : dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable bool true ");
	    LOGGER.info(
		    "STEP 3: EXPECTED : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable should be set true");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DISABLE_STRICT_DNS_ORDER,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.DNSStrictOrder.Enable successfully set as TRUE");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S4";
	    errorMessage = "Failed to enable DNS scrict order feature in this device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Restart the dnsmasq service and verify whether the feature got enabled or not ");
	    LOGGER.info(
		    "STEP 4: ACTION : 1) systemctl restart dnsmasq\n 2) grep -I \"Starting dnsmasq with additional dns strict order option: true\" /rdklogs/logs/Consolelog.txt.0 or ArmConsole.txt");
	    LOGGER.info(
		    "STEP 4: EXPECTED : After restarting dnsmasq service DNS strict order option should get set to true in /rdklogs/logs/Consolelog.txt.0 file or ArmConsole.txt");
	    LOGGER.info("**********************************************************************************");

	    CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_ENABLED,
				BroadBandCommandConstants.FILE_ARMCONSOLELOG));
	    } else if (DeviceModeHandler.isFibreDevice(device)) {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_ENABLED,
				BroadBandCommandConstants.FILE_ATOM_JOURNALLOG));
	    } else {
		status = CommonUtils.searchLogFiles(tapEnv, device,
			CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
				BroadBandTraceConstants.LOG_MSG_FOR_DNS_STRICT_ORDER_ENABLED,
				BroadBandCommandConstants.FILE_CONSOLELOG));
	    }

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Successfully verified that DNS strict order feature has been enabled in the device");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S5";
	    errorMessage = "Failed to start analyzing packets transferred from port 53 using tcpdump command";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Execute tcpdump command and try pinging valid url and verify /tmp/dummy.txt file is created or not");
	    LOGGER.info(
		    "STEP 5: ACTION : 1) /usr/sbin/tcpdump -i any -n udp port 53 &> /tmp/dummy.txt &\n 2) ping6 -c 10 www.google.com\n3) killall tcpdump\n4) ps -ef | grep [t]tcpdump");
	    LOGGER.info(
		    "STEP 5: EXPECTED : No tcpdump process should be running and verify dummy.txt  is present under /tmp");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadbandPropertyFileHandler.getTCPDUMPFile(),
			BroadBandCommandConstants.FOLDER_PATH_TMP);
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PERMISSION_TO_TCPDUMP);
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_START_TCPDUMP_FOR_PORT_53_FOR_CISCO);
	    } else {
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_START_TCPDUMP_FOR_PORT_53);
	    }
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_PING_GOOGLE_COM);
	    tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandCommandConstants.TCPDUMP);

	    if (CommonMethods.isNull(CommonUtils.getPidOfProcess(device, tapEnv, BroadBandCommandConstants.TCPDUMP))) {
		status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP);
	    } else {
		errorMessage = "Failed to capture UDP packets transferred using tcpdump command";
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully taken tcpdump in /tmp/dummy.txt file");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S6";
	    errorMessage = "Failed to send packets first to primary server";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify whether the DNS queries sent to only primary server");
	    LOGGER.info("STEP 6: ACTION : 1) grep -I \"google.com\" /tmp/dummy.txt");
	    LOGGER.info("STEP 6: EXPECTED : In the response only primary DNS server details should present");
	    LOGGER.info("**********************************************************************************");

	    scanFile = new Scanner(tapEnv.executeCommandUsingSsh(device,
		    RDKBTestConstants.CAT_COMMAND + BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));
	    scanFile.next();// to skip domain word in that file
	    scanFile.next();// to skip word in that file
	    scanFile.next();// to skip the nameserver word in that file
	    dns_server_1 = scanFile.next();
	    scanFile.next();// to skip the nameserver word in that file
	    dns_server_2 = scanFile.next();
	    LOGGER.info("server 1 : " + dns_server_1);
	    LOGGER.info("server 2 : " + dns_server_2);

	    response = tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
			    BroadBandCommandConstants.URL_GOOGLE, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP));

	    status = CommonMethods.patternMatcher(response, dns_server_1)
		    && !CommonMethods.patternMatcher(response, dns_server_2);

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Successfully verified that all DNS quries are going to only DNS server 1 not to DNS server 2");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S7";
	    errorMessage = "Failed to use -o option by DNS service";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify whether the DNS service uses -o option or not");
	    LOGGER.info("STEP 7: ACTION : ps | grep -i dnsmasq");
	    LOGGER.info(
		    "STEP 7: EXPECTED : Response should contain -o option as \"dnsmasq -u nobody -q --clear-on-reload --bind-dynamic --add-mac --add-cpe-id=abcdefgh -P 4096 -C /var/dnsmasq.conf -o\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_FETCH_DNSMASQ_SERVICE_DETAILS);
	    status = CommonMethods.patternMatcher(response, BroadBandCommandConstants.STRING_CONST_HYPHEN_O);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified that dnsmasq service uses -o option");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S8";
	    errorMessage = "Unable to assign junk ip to dns server in /etc/resolv.dnsmasq file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Assign junk ip to DSN server1");
	    LOGGER.info("STEP 8: ACTION : 1) /sbin/mount-copybind /tmp/resolve.conf /etc/resolve.conf\n"
		    + "2) Change the name server1\"s ip ");
	    LOGGER.info("STEP 8: EXPECTED : Check whether the changes been reflected in /etc/reslov.conf file also");
	    LOGGER.info("**********************************************************************************");

	    // cp /etc/resolv.dnsmasq /tmp/resolv.dnsmasq
	    tapEnv.executeCommandUsingSsh(device, CommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_COPY_FILES, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_DSTN_PATH));

	    // sed -i '1\\ s/\\$/0/g' /tmp/resolv.dnsmasq
	    dns_server_1 = BroadBandTestConstants.INVALID_DNS_SERVER;
	    dns_server_2 = BroadBandTestConstants.DNS_SERVER_2;
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_DEL_ALL_LINES_EXCEPT_FIRST);
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_ADD_INVALID_PRI_DNS_SERVER);
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_ADD_VALID_SEC_DNS_SERVER);

	    // sbin mount-copybind /tmp/resolv.dnsmasq /etc/reslov.dnsmasq
	    tapEnv.executeCommandUsingSsh(device, CommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.SBIN_MOUNT_COPYBIND, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_DSTN_PATH, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
		    BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH));
	    // invalid ip in resolv.conf
	    tapEnv.executeCommandUsingSsh(device,
		    RDKBTestConstants.CAT_COMMAND + BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH);

	    status = CommonUtils.searchLogFiles(tapEnv, device,
		    RDKBTestConstants.GREP_COMMAND + dns_server_1 + RDKBTestConstants.SINGLE_SPACE_CHARACTER
			    + BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_DSTN_PATH);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully assigned junk ip to DNS server 1 in /etc/reslov.conf file");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S9";
	    errorMessage = "Failed to take tcpdump into /tmp/dummy.txt file using tcpdump command for negative scenario";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Start tcpdump monitoring and try pinging valid url and verify");
	    LOGGER.info(
		    "STEP 9: ACTION : 1) /usr/sbin/tcpdump -i any -n udp port 53 &> /tmp/dummy.txt &2) ping6 -c 3 google.com3) killall tcpdump");
	    LOGGER.info("STEP 9: EXPECTED : Verify dummy.txt is present under /tmp");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadbandPropertyFileHandler.getTCPDUMPFile(),
			BroadBandCommandConstants.FOLDER_PATH_TMP);
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_PERMISSION_TO_TCPDUMP);
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_START_TCPDUMP_FOR_PORT_53_FOR_CISCO);
	    } else {
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_START_TCPDUMP_FOR_PORT_53);
	    }
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_PING_GOOGLE_COM);
	    tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_KILL_ANY_PROCESS + BroadBandCommandConstants.TCPDUMP);

	    if (CommonMethods.isNull(CommonUtils.getPidOfProcess(device, tapEnv, BroadBandCommandConstants.TCPDUMP))) {
		status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP);
	    } else {
		errorMessage = "Failed to capture UDP packets transferred using tcpdump command";
	    }

	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Successfully taken tcpdump in /tmp/dummy.txt file for negative scenario(falling back to DNS server 2)");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S10";
	    errorMessage = "Failed to connect to DNS server1 and then fallback to server2";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify whether the DNS queries first sent to primary wrong URL");
	    LOGGER.info("STEP 10: ACTION : 1) grep -I \"google.com\" /tmp/dummy.txt");
	    LOGGER.info(
		    "STEP 10: EXPECTED : In the response invalid ip address should be present(2001:558:feed::10 ip)");
	    LOGGER.info("**********************************************************************************");

	    tapEnv.executeCommandUsingSsh(device,
		    RDKBTestConstants.CAT_COMMAND + BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP);
	    response = tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(RDKBTestConstants.GREP_COMMAND,
			    BroadBandCommandConstants.URL_GOOGLE, RDKBTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP));
	    status = CommonMethods.patternMatcher(response, dns_server_1);

	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL : Successfully verified that DNS queries first sent to DNS server 1 using wrong IP and got failed");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S11";
	    errorMessage = "Failed to fallback to DNS server2 when DNS server1 is unreachable";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify whether the DNS queries then fallback to secondary when the primary is not available");
	    LOGGER.info("STEP 11: ACTION : 1) grep -I \"google.com\" /tmp/dummy.txt");
	    LOGGER.info("STEP 11: EXPECTED : response should contain secondary dns server name ( 2001:558:feed::2 ip)");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.patternMatcher(response, dns_server_2);

	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL : Successfully verified that when the DNS server 1 is not reachable all queries fallen back to DNS server 2");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "S12";
	    errorMessage = "Fallback failed to happen within 5s";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION : Verify whether the DNS queries then fallback to secondary within 5 sec");
	    LOGGER.info("STEP 12: ACTION : calculate time diff between primary and secondary fall back");
	    LOGGER.info(
		    "STEP 12: EXPECTED : In the response should contain time stamp within 5s of difference between fallback");
	    LOGGER.info("**********************************************************************************");

	    // gets the time stamp of line that contains server1
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, dns_server_1,
		    BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    LOGGER.info("server1 response " + response);
	    if (CommonMethods.isNotNull(response)) {
		// get the time stamp from the response output
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TIME_FORMAT);
		LOGGER.info("server1 hit time " + response);
		if (CommonMethods.isNotNull(response)) {
		    // convert the time stamp to formated output
		    try {
			firstserverhitTime = format.parse(response);
		    } catch (ParseException e) {
			LOGGER.error(e.getMessage());
		    }
		}
	    }

	    // gets time stamp of line that contains server2
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, dns_server_2,
		    BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);
	    LOGGER.info("server2 response " + response);
	    if (CommonMethods.isNotNull(response)) {
		// get the time stamp from the response output
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_TIME_FORMAT);
		LOGGER.info("server2 hit time " + response);
		if (CommonMethods.isNotNull(response)) {
		    // convert the time stamp to formated output
		    try {
			secondserverhitTime = format.parse(response);
		    } catch (ParseException e) {
			LOGGER.error(e.getMessage());
		    }
		}
	    }

	    // calculate the differrence in time
	    if (firstserverhitTime != null && secondserverhitTime != null) {
		timeDifference = secondserverhitTime.getTime() - firstserverhitTime.getTime();
		LOGGER.info("time difference for fallback from primary server to secondary server : " + timeDifference);
	    }

	    // status true if time is < 5s
	    status = (timeDifference <= BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Successfully verified that the fallback happens within 5s");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Remove the rfc configurations and /tmp/dummy.txt file");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : 1) umount /etc/resolv.conf \n  2)set dns strict to false 3) rm -f /tmp/dummy.txt");
	    LOGGER.info(
		    "POST-CONDITION : EXPECTED : Verify whether the changes made in step7 reverted back to original or not");

	    tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_TO_UNMOUNT_FILE_SYSTEM + RDKBTestConstants.SINGLE_SPACE_CHARACTER
			    + BroadBandCommandConstants.FILE_RESOLV_DNSMASQ_SRC_PATH);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandCommandConstants.DUMMY_FILE_PATH_IN_TMP);
	    BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_ENABLE_DISABLE_STRICT_DNS_ORDER,
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    BroadBandCommonUtils.killProcessAndVerify(device, tapEnv, BroadBandTestConstants.STRING_DNSMASQ,
		    BroadBandTestConstants.PATTERN_PROCESS_DNSMASQ);
	    long startTime = System.currentTimeMillis();
	    do {
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
			BroadBandTestConstants.STRING_DNSMASQ);
		tapEnv.waitTill(RDKBTestConstants.FIFTEEN_SECONDS_IN_MILLIS);
	    } while (!status && (System.currentTimeMillis() - startTime) < RDKBTestConstants.FIVE_MINUTES);

	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-3048");
    }

    /**
     * Helper method to verify atom console execution response from arm execution
     * 
     * @param device
     *            - {@link Instanceof} Dut
     * @param tapEnv
     *            - {@link Instanceof} AutomaticsTapApi
     * @param commands
     *            - commands to be executed to check the atom access
     * @param expectedString
     *            - expected response for command execution
     * @param console
     *            - console details
     * 
     * @return broadBandResultObject object with validation details
     * @Refactor Sruthi Santhosh
     * 
     */
    private BroadBandResultObject verifyAccessLogsFromArmConsole(Dut device, AutomaticsTapApi tapEnv, String[] commands,
	    String expectedString, String console) {

	boolean status = false;
	String response = null;
	String errorMessage = BroadBandTestConstants.EMPTY_STRING;
	BroadBandResultObject broadBandResultObject = new BroadBandResultObject();

	response = tapEnv.executeCommandUsingSsh(device, commands);

	if (CommonMethods.isNotNull(response)) {

	    LOGGER.info(
		    "Obtained response for command execution to check " + console + " access from arm : " + response);
	    status = CommonUtils.patternSearchFromTargetString(response, expectedString);

	    if (!status) {
		errorMessage = "Failed to access " + console
			+ " console from arm console. Obtained response for command execution is : " + response;
	    }

	} else {
	    errorMessage = "Obtained null response for when we execute command for accessing " + console
		    + " from arm console !!!";
	    LOGGER.error(errorMessage);
	}

	broadBandResultObject.setStatus(status);
	broadBandResultObject.setErrorMessage(errorMessage);

	return broadBandResultObject;

    }

    /**
     * Average usage must be computed over 15 minute time window
     * <ol>
     * <li>Pre-Condition:Factory reset the device to check default values</li>
     * <li>Verification of usage compute window using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow to get the default
     * UsageComputeWindow value</li>
     * <li>Verification of adding invalid special characters with usage compute window using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow</li>
     * <li>Verification of adding invalid value with usage compute window using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow</li>
     * <li>Factory reset the device to check default values</li>
     * <li>Verification of usage compute window using the TR181
     * parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow to get the default
     * UsageComputeWindow value</li>
     * </ol>
     * 
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SELF-HEAL-ACW-3001")
    public void validateAvgCpu(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SELF-HEAL-ACW-301";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	int preConStepNumber = 0;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SELF-HEAL-ACW-3001");
	LOGGER.info("TEST DESCRIPTION: Average usage must be computed over 15 minute time window");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre-Condition:Factory reset the device to check default values");
	LOGGER.info(
		"1. Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
	LOGGER.info(
		"2. Verification of adding invalid special characters with usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow ");
	LOGGER.info(
		"3. Verification of adding invalid value with usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow");
	LOGGER.info("4. Factory reset the device to check default values");
	LOGGER.info(
		"5. Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
	LOGGER.info("#######################################################################################");

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Factory reset the device to check default values");
	    LOGGER.info("PRE-CONDITION : ACTION : Performing factory reset by webpa");
	    LOGGER.info("PRE-CONDITION : EXPECTED : The device should get factory resetted by webpa");
	    LOGGER.info("#######################################################################################");

	    status = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    preConStepNumber);
	    if (status) {
		LOGGER.info("PRE-CONDITION : ACTUAL : Pre condition executed successfully");
	    } else {
		LOGGER.error("PRE-CONDITION : ACTUAL : Pre condition failed");
	    }
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S1";
	    errorMessage = "Unable to get resourceUsageComputeWindow  Value.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the Webpa get command on  Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow  to get the resourceUsageComputeWindow  .   ");
	    LOGGER.info("STEP 1: EXPECTED : UsageComputeWindow should have the default value as 15");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
	    LOGGER.info("UsageComputeWindow retrieved using WebPa = " + response);
	    status = CommonMethods.isNotNull(response)
		    && response.equalsIgnoreCase(BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty());
	    errorMessage = "Unable to verify the UsageComputeWindow  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow'  -Expected value:"
		    + BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:" + response;

	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL :Successfully verified the default UsageComputeWindow  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow' using WebPA command.-Expected value:"
				+ BroadBandTestConstants.CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW + "|Actual Value:"
				+ response);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "Able to set resourceUsageComputeWindow  Value.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verification of adding invalid special characters with usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow ");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the Webpa get command on  Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow  to get the resourceUsageComputeWindow  .   ");
	    LOGGER.info("STEP 2: EXPECTED : UsageComputeWindow should not set invalid special charaters");
	    LOGGER.info("**********************************************************************************");
	    boolean checkSuccessStatus = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.INVALID_SPEC_CHAR_CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW);

	    if (!checkSuccessStatus) {
		status = true;
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL :UsageComputeWindow has been set with invalid special charaters");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Unable to set resourceUsageComputeWindow Value.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verification of adding invalid value with usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the Webpa get command on  Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow  to get the resourceUsageComputeWindow  .   ");
	    LOGGER.info("STEP 3: EXPECTED : UsageComputeWindow should have the invalid value as 16");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, BroadBandTestConstants.CONSTANT_2,
		    BroadBandTestConstants.INVALID_CONSTANT_RESOURCE_USAGE_COMPUTE_WINDOW);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : UsageComputeWindow has been set invalid value as 16");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "The device should not get factory resetted by webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Factory reset the device to check default values");
	    LOGGER.info("STEP 4: ACTION : Performing factory reset by webpa");
	    LOGGER.info("STEP 4: EXPECTED : The device should get factory resetted by webpa");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully Factory Reset Device");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = "Unable to set resourceUsageComputeWindow  Value.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Verification of usage compute window using the TR181 parameter-Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow  to get the default UsageComputeWindow value");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the Webpa get command on  Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow  to get the resourceUsageComputeWindow  .   ");
	    LOGGER.info("STEP 5: EXPECTED : UsageComputeWindow should have the default value as 15");
	    LOGGER.info("**********************************************************************************");
	    long startTime = System.currentTimeMillis();
	    do {
		response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW);
		LOGGER.info("UsageComputeWindow retrieved using WebPa = " + response);
		status = CommonMethods.isNotNull(response)
			&& response.equalsIgnoreCase(BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty());
	    } while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    errorMessage = "Unable to verify the UsageComputeWindow  using WebPA command on TR181 parameter 'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow'  -Expected value:"
		    + BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty() + "|Actual Value:" + response;
	    if (status) {
		LOGGER.info(
			"STEP 5: ACTUAL : Successfully verified the default UsageComputeWindow  'Device.SelfHeal.ConnectivityTest.X_RDKCENTRAL-COM_UsageComputeWindow' using WebPA command.-Expected value:"
				+ BroadbandPropertyFileHandler.getResourceUsageComputeWindowFromProperty() + "|Actual Value:"
				+ response);
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SELF-HEAL-ACW-3001");
    }

    /**
     * 
     * This is to verify the encryption of /opt/secure folder. Upon unmounting the folder the contents of the folder
     * should be encrypted. Thus it prevents the unauthorized alteration.
     * 
     * Common implementation for nvram init to eliminate dependencies and patches (productization)
     * <li>Steps:</li>
     * <ol>
     * STEP1:Verify whether the mount is secured for the folder /opt/secure
     * </ol>
     * <ol>
     * STEP 2:Create a temporary file under /opt/secure
     * <ol>
     * STEP 3: Verify that securemount.service is in active state
     * </ol>
     * <ol>
     * STEP 4:Verify the presence of three backup files under /opt
     * </ol>
     * <ol>
     * STEP 5:Remove any one of the three backup files under /opt
     * </ol>
     * <ol>
     * STEP 6:Unmount /opt/secure
     * </ol>
     * <ol>
     * STEP 7:Verify whether /opt/secure folder is unmounted or not
     * </ol>
     * <ol>
     * STEP 8:Verify whether the temporary file name is readable
     * </ol>
     * <ol>
     * STEP 9:Verify whether the contents of the temp file is readable
     * </ol>
     * <ol>
     * STEP 10:Restart the securemount service to mount secure folder
     * </ol>
     * <ol>
     * STEP 11:Verify whether the mount is secured again after reboot
     * </ol>
     * <ol>
     * STEP 12:Verify whether the temporary file name is readable after reboot
     * </ol>
     * <ol>
     * STEP 13:Verify whether the contents of the temporary file is readable
     * </ol>
     * <ol>
     * STEP 14:Verify that the three backup files are created once again after restarting securemount service
     * </ol>
     * <ol>
     * STEP 15 : Verify the logs 'Mounted cpc directory succssfully' in case of successful mounting of /opt/secure
     * </ol>
     * <ol>
     * STEP 16 : Restart securemount service when /opt/secure is already mounted
     * </ol>
     * <ol>
     * STEP 17 : Verify that error log 'Failed to mount cpc directory' is observed on restarting securemount service
     * when /opt/secure is already mounted
     * </ol>
     * <ol>
     * STEP 18 : Unmount /opt/secure once again
     * </ol>
     * <ol>
     * STEP 19 : Remove all the three backup files under /opt
     * </ol>
     * <ol>
     * STEP 20 : Reboot the device to mount secure folder
     * </ol>
     * <ol>
     * STEP 21 : Verify that previous contents of /opt/secure are no longer available
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
     * @author Sumathi Gunasekaran
     * @refactor yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-3042")
    public void testToVerifyEncryptionOfOptSecureFolder(Dut device) {
	// boolean variable to store the status
	boolean status = false;
	// Test case id
	String testId = "TC-RDKB-SYSTEM-042";
	// Test step number
	String testStepNumber = "s1";
	// error message
	String errorMessage = null;
	// response to store the result after executing stb commands
	String response = null;
	// variable to store the encrypted name of the temporary file
	String encryptedFileName = null;
	BroadBandResultObject result = new BroadBandResultObject();
	boolean isFibreDevice = DeviceModeHandler.isFibreDevice(device);

	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-3042 ");
	LOGGER.info("**************************************************************");
	LOGGER.info("TEST DESCRIPTION: Test to verify encryption of /opt/secure folder ");
	LOGGER.info("*************************************************************************");
	LOGGER.info("STEP 1:Verify whether the mount is secured for the folder /opt/secure");
	LOGGER.info("EXPECTED:Should display that /opt/secure is mounted as type ecryptfs");
	LOGGER.info("STEP 2:Create a temporary file under /opt/secure");
	LOGGER.info("EXPECTED:The sample.txt file must be created under /opt/secure");
	LOGGER.info("STEP 3:Verify that securemount.service is in active state");
	LOGGER.info("EXPECTED:securemount service should be in active state");
	LOGGER.info("STEP 4:Verify the presence of three backup files under /opt");
	LOGGER.info("EXPECTED:All the three backup files should be present under /opt");
	LOGGER.info("STEP 5:Remove any one of the three backup files under /opt");
	LOGGER.info("EXPECTED:File removal should be successful");
	LOGGER.info("STEP 6:Unmount /opt/secure");
	LOGGER.info("EXPECTED:the folder should be unmounted without any issue");
	LOGGER.info("STEP 7:Verify whether /opt/secure folder is unmounted or not");
	LOGGER.info("EXPECTED:executing  \"mount|grep secure\" command should display nothing");
	LOGGER.info("STEP 8:Verify that the temporary file name(sample.txt created in step 2) is not readable");
	LOGGER.info("EXPECTED:The temporary file name should not be in a readable format and it must be encrypted");
	LOGGER.info("STEP 9:Verify whether the contents of the temp file is readable");
	LOGGER.info("EXPECTED:The contents should not be in a readable format and it must be encrypted");
	LOGGER.info("STEP 10: Restart the securemount service to mount secure folder");
	LOGGER.info("EXPECTED:securemount service should restarted successfully");
	LOGGER.info("STEP 11:Verify that the /opt/secure is mounted again after restarting securemount service");
	LOGGER.info(
		"EXPECTED:\"mount | grep secure\" command Should list /opt/secure folder as mounted with type ecryptfs");
	LOGGER.info("STEP 12:Verify whether the temporary file name(sample.txt) is readable after restart");
	LOGGER.info("EXPECTED:The temporary file name should be in a readable format");
	LOGGER.info("STEP 13:Verify whether the contents of the temporary file(sample.txt) is readable");
	LOGGER.info("EXPECTED:The contents of temporary file created should be readable");
	LOGGER.info(
		"STEP 14:Verify that the three backup files are created once again after restarting securemount service");
	LOGGER.info("EXPECTED:All the three backup files should be present under /opt");
	LOGGER.info(
		"STEP 15 : Verify the logs 'Mounted cpc directory succssfully' in case of successful mounting of /opt/secure");
	LOGGER.info("Expected: Expected logs should be present");
	LOGGER.info("STEP 16 : Restart securemount service when /opt/secure is already mounted");
	LOGGER.info("Expected: securemount service should be restarted successfully");
	LOGGER.info(
		"STEP 17 : Verify that error log 'Failed to mount cpc directory' is observed on restarting securemount service when /opt/secure is already mounted");
	LOGGER.info("Expected: Expected log line should be present");
	LOGGER.info("STEP 18 : Unmount /opt/secure once again");
	LOGGER.info("Expected: /opt/secure folder should be unmounted without any issue");
	LOGGER.info("STEP 19 : Remove all the three backup files under /opt");
	LOGGER.info("Expected: File removal should be successful");
	LOGGER.info("STEP 20 : Reboot the device to mount secure folder");
	LOGGER.info("Expected: Device should be rebooted successfully");
	LOGGER.info("STEP 21 : Verify that previous contents of /opt/secure are no longer available");
	LOGGER.info(
		"Expected: sample.txt file should not be present as all the backup files used to create encryption keys were deleted and new key got created upon restarting securemount");
	try {
	    LOGGER.info("############################# STARTING PRE-CONFIGURATIONS #############################");
	    if (isFibreDevice) {
		BroadBandPreConditionUtils.preConditionToRebootAndWaitForIpAccusition(device, tapEnv,
			BroadBandTestConstants.CONSTANT_1);
	    }
	    LOGGER.info("############################# COMPLETED PRE-CONFIGURATIONS #############################");

	    errorMessage = "/opt/secure folder is NOT mounted as type ecryptfs";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: Verify whether the mount is secured for the folder /opt/secure");
	    LOGGER.info("STEP 1: ACTION: Execute command:: mount | grep opt");
	    LOGGER.info("STEP 1: EXPECTED: Should display that /opt/secure is mounted as type ecryptfs");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("Removing the /opt/logs/ecfs.txt file to clear previous entries");
	    CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device, BroadBandCommandConstants.FILE_ECFS_TXT);
	    if (isFibreDevice) {
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(
				BroadBandTestConstants.CMD_TO_LIST_ALL_MOUNTED_FILES,
				BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.GREP_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SECURE_FOLDER));
	    } else {
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(
				BroadBandTestConstants.CMD_TO_LIST_ALL_MOUNTED_FILES,
				BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.GREP_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.TEXT_OPT));
	    }
	    if (CommonMethods.isNotNull(response)) {
		status = (CommonMethods.isAtomSyncAvailable(device, tapEnv))
			? (CommonMethods.patternMatcher(response, BroadBandTestConstants.OPT_SECURE_SAFE_MOUNT_RESPONSE)
				&& CommonMethods.patternMatcher(response,
					BroadBandCommandConstants.FILE_PATH_DEV_MMCBLK0P7))
			: CommonMethods.patternMatcher(response, BroadBandTestConstants.OPT_SECURE_SAFE_MOUNT_RESPONSE);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully verified that /opt/secure is mounted as type encryptfs");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s2";
	    status = false;
	    errorMessage = "sample.txt file is NOT created in /opt/secure folder";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Create a temporary file under /opt/secure");
	    LOGGER.info(
		    "STEP 2: ACTION: Execute command:: echo \"This is for encryption check\" > /opt/secure/sample.txt");
	    LOGGER.info("STEP 2: Expected: The sample.txt file must be created under /opt/secure");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.executeCommandUsingSsh(device, CommonMethods.concatStringUsingStringBuffer(
		    BroadBandTestConstants.ECHO_WITH_SPACE, BroadBandTestConstants.TEXT_TO_TEMP_FILE,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.CHR_CLOSING_ANGLE_BRACKET,
		    BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.FILE_PATH_SECURE_FOLDER,
		    BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY));
	    LOGGER.info("Verifying whether the sample file is created");
	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_PATH_SECURE_FOLDER
		    + BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully verified sample text file is created under /opt/secure");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s3";
	    status = false;
	    errorMessage = "securemount service is not in active state";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Verify that securemount.service is in active state");
	    LOGGER.info("STEP 3: ACTION: Execute command:: systemctl status securemount");
	    LOGGER.info("STEP 3: EXPECTED: securemount service should be in active state");
	    LOGGER.info("**********************************************************************************");
	    if (!(CommonMethods.isAtomSyncAvailable(device, tapEnv)) && !(isFibreDevice)) {
		status = CommonUtils.getServiceStatus(device, tapEnv, BroadBandTestConstants.SECUREMOUNT_SERVICE)
			.equalsIgnoreCase(BroadBandTestConstants.STATUS_ACTIVE);
		if (status) {
		    LOGGER.info("STEP 3: ACTUAL : Successfully verified that securemount service is in active state");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    } else if (isFibreDevice) {
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.FILE_PATH_ECFS_TXT));
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTraceConstants.LOG_TO_VERIFY_OPT_SECURE_IS_MOUNTED);
		if (status) {
		    LOGGER.info("STEP 3: ACTUAL : Successfully verified that mounting is successful");
		} else {
		    LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	    } else {
		LOGGER.info(
			"STEP :3  This Step is not applicable for Atom Console Devices, Since the feature is available only for ARM side");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s4";
	    status = false;
	    errorMessage = "Unable to verify the presence of all three backup files under /opt";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Verify the presence of three backup files under /opt");
	    LOGGER.info(
		    "STEP 4: ACTION: Execute command:: if [ -f /opt/<filename> ] ; then echo \"true\" ; else echo \"false\" ; fi");
	    LOGGER.info("STEP 4: EXPECTED: All the three backup files should be present under /opt");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSystemUtils.verifyBackupFilesArePresentForEncryption(device, tapEnv);
	    errorMessage = result.getErrorMessage();
	    status = result.isStatus();

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified the presence of all three backup files under /opt");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s5";
	    status = true;
	    errorMessage = "Unable to remove backup file /nvram/<backup file>";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION: Remove any one or two of the three backup files <backup file>, <backup file> and <backup file> under /opt");
	    LOGGER.info("STEP 5: ACTION: Execute command:: 1) rm -rf /nvram/<backup file>");
	    LOGGER.info("STEP 5: EXPECTED: File removal should be successful");
	    LOGGER.info("**********************************************************************************");
	    String files = BroadbandPropertyFileHandler.getSecureMountBackUpFiles();
	    String[] backup_files = files.split(",");
	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.MOUNT_NVRAM,
			    backup_files[BroadBandTestConstants.CONSTANT_0]));
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully removed backup files under /opt");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s6";
	    status = false;
	    errorMessage = "response is not null after executing command umount /opt/secure which implies /opt/secure folder is not unmounted successfully";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Unmount /opt/secure");
	    LOGGER.info("STEP 6: ACTION: Execute command :: umount /opt/secure");
	    LOGGER.info(
		    "STEP 6: Expected: /opt/secure folder should be unmounted without any issue and command response should be null");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_TO_UNMOUNT_FILE_SYSTEM,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER.substring(BroadBandTestConstants.CONSTANT_0,
				    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER.length()
					    - BroadBandTestConstants.CONSTANT_1))));
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully unmounted /opt/secure folder");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s7";
	    status = false;
	    errorMessage = "Executing mount | grep secure after unmounting /opt/secure folder do NOT result in null response which implies /opt/secure folder is NOT unmounted properly";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Verify whether /opt/secure folder is unmounted or not");
	    LOGGER.info("STEP 7: ACTION: Execute command :: mount | grep secure");
	    LOGGER.info(
		    "STEP 7: Expected: Executing  \"mount|grep secure\" command should display nothing or null response");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(
				BroadBandTestConstants.CMD_TO_LIST_ALL_MOUNTED_FILES,
				BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.GREP_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SECURE_FOLDER));
		status = CommonMethods.isNull(response);

	    } else {
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(
				BroadBandTestConstants.CMD_TO_LIST_ALL_MOUNTED_FILES,
				BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.GREP_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SECURE_FOLDER));
		status = CommonMethods.isNotNull(response) && !CommonMethods.patternMatcher(response,
			BroadBandTestConstants.OPT_SECURE_SAFE_MOUNT_RESPONSE);
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified that /opt/secure is unmounted");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s8";
	    status = false;
	    errorMessage = "Name of file sample.txt is NOT encrypted after unmounting /opt/secure folder ";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION: Verify that the temporary file NAME created in step 2 is not readable");
	    LOGGER.info(
		    "STEP 8: ACTION: Execute command:: ls -la /opt/secure and verify that response should not contain sample.txt and should contain a file starting with ECRYPTFS_FNEK_ENCRYPTED");
	    LOGGER.info(
		    "STEP 8: Expected: The temporary file name should not be in a readable format and it must be encrypted");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_TO_LONGLIST_FOLDER_FILES,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER));
	    // response should not contain sample.txt and it must contain a file
	    // starting with string "ECRYPTFS_FNEK_ENCRYPTED"
	    status = CommonMethods.isNotNull(response)
		    && !CommonMethods.patternMatcher(response, BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.ENCRYPTED_FILE_STRING);
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : Successfully verified the sample.txt file created in step 2 is not in readable format");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s9";
	    status = false;
	    errorMessage = "Unable to verify that the content of sample.txt file is not readable";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: Verify that the contents of the sample.txt file is not readable");
	    LOGGER.info(
		    "STEP 9: ACTION: Execute command:: 1) ls -t /opt/secure/ | grep ECRYPTFS_FNEK_ENCRYPTED | head -n1 - to get the file name 2) cat /opt/secure/<filename> and verify that content is not readable");
	    LOGGER.info("STEP 9: EXPECTED :The contents should not be in a readable format and it must be encrypted");
	    LOGGER.info("Executing command to obtain the name of encrypted file");
	    LOGGER.info("**********************************************************************************");
	    encryptedFileName = tapEnv.executeCommandUsingSsh(device,
		    CommonMethods.concatStringUsingStringBuffer(CMD_LIST_FILES_ORDER_BY_DATE,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER, BroadBandTestConstants.SYMBOL_PIPE,
			    BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.ENCRYPTED_FILE_STRING,
			    BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES, CMD_GET_HEAD_FILE));
	    LOGGER.info("Encrypted file name=" + encryptedFileName);
	    errorMessage = "Unable to obtain the encrypted file name using command:: ls -t /opt/secure/ | grep ECRYPTFS_FNEK_ENCRYPTED | head -n1 ";
	    if (CommonMethods.isNotNull(encryptedFileName)) {
		// executing command to read the contents of encrypted file
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_LINUX_CAT,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.FILE_PATH_SECURE_FOLDER, encryptedFileName));
		errorMessage = "Unable to verify cat command response. Either encrypted file name was not obtained correctly or it does not exist";
		if (CommonMethods.isNotNull(response) && !(CommonMethods.patternMatcher(response,
			BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY))) {
		    LOGGER.info("Verifying that the content of encrypted file is not readable");
		    errorMessage = "Contents of Temporary file are NOT encrypted after unmounting /opt/secure folder ";
		    status = !CommonMethods.patternMatcher(response, BroadBandTestConstants.TEXT_TO_TEMP_FILE);
		}
	    }
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Successfully verified that the content of sample.txt file created in step 2 is not in readable format");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    if (!(CommonMethods.isAtomSyncAvailable(device, tapEnv))) {
		if (isFibreDevice) {
		    testStepNumber = "s10";
		    status = false;
		    errorMessage = "Device could not be rebooted successfully";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP 10: DESCRIPTION: Reboot the device to mount secure folder for Fiber Devices");
		    LOGGER.info("STEP 10: ACTION: Execute command:: reboot");
		    LOGGER.info("STEP 10: EXPECTED: Device should be rebooted successfully");
		    LOGGER.info("**********************************************************************************");
		    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
		    if (status) {
			LOGGER.info(
				"STEP 10: ACTUAL : Successfully rebooted the device to securemount service for Fiber Devices");
		    } else {
			LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		} else {
		    testStepNumber = "s10";
		    status = false;
		    errorMessage = "securemount service could not be restarted successfully";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info("STEP 10: DESCRIPTION: Restart the securemount service to mount secure folder");
		    LOGGER.info("STEP 10: ACTION: Execute command:: systemctl restart securemount");
		    LOGGER.info("STEP 10: EXPECTED: securemount service should restarted successfully");
		    LOGGER.info("**********************************************************************************");
		    status = CommonUtils.restartServiceAndVerifyServiceStatus(device, tapEnv,
			    BroadBandTestConstants.SECUREMOUNT_SERVICE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		    if (status) {
			LOGGER.info("STEP 10: ACTUAL : Successfully restarted securemount service");
		    } else {
			LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
		}

		testStepNumber = "s11";
		status = false;
		errorMessage = "/opt/secure folder is NOT mounted securely after restarting securemount service";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 11: DESCRIPTION: Verify that the /opt/secure is mounted after restarting securemount service");
		LOGGER.info("STEP 11: ACTION: Execute command:: mount | grep secure");
		LOGGER.info("STEP 11: EXPECTED: Should display that /opt/secure is mounted as type ecryptfs");
		LOGGER.info("**********************************************************************************");
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(
				BroadBandTestConstants.CMD_TO_LIST_ALL_MOUNTED_FILES,
				BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.GREP_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.SECURE_FOLDER));
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			BroadBandTestConstants.OPT_SECURE_SAFE_MOUNT_RESPONSE);
		if (status) {
		    LOGGER.info(
			    "STEP 11: ACTUAL : Successfully verified that /opt/secure is mounted as type encryptfs");
		} else {
		    LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		testStepNumber = "s12";
		status = false;
		errorMessage = "Failed to find the temporary file sample.txt file after restarting securemount service ";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 12: DESCRIPTION: Verify that the temporary file name(created in step 2) is readable after restarting securemount service");
		LOGGER.info(
			"STEP 12: ACTION: Execute command:: ls -la /opt/secure and verify that response should not contain sample.txt and should contain a file starting with ECRYPTFS_FNEK_ENCRYPTED");
		LOGGER.info("STEP 12: EXPECTED :The temporary file name should be in a readable format");
		LOGGER.info("**********************************************************************************");
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_TO_LONGLIST_FOLDER_FILES,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.FILE_PATH_SECURE_FOLDER, BroadBandTestConstants.SYMBOL_PIPE,
				BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY));
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY);
		if (status) {
		    LOGGER.info(
			    "STEP 12: ACTUAL : Successfully verified that sample.txt file exists under /opt/secure location");
		} else {
		    LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		testStepNumber = "s13";
		status = false;
		errorMessage = "Unable to verify that the content of sample.txt file is readable";
		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 13: DESCRIPTION: Verify that the contents of the sample.txt file is readable now");
		LOGGER.info(
			"STEP 13: ACTION: Execute command:: cat /opt/secure/sample.txt and verify that content is readable");
		LOGGER.info("STEP 13: EXPECTED: Content of sample.txt should be in a readable format");
		LOGGER.info("**********************************************************************************");
		response = tapEnv.executeCommandUsingSsh(device,
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_LINUX_CAT,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.FILE_PATH_SECURE_FOLDER,
				BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY));
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.TEXT_TO_TEMP_FILE);
		if (status) {
		    LOGGER.info(
			    "STEP 13: ACTUAL : Successfully verified that the content of sample.txt file is now readable");
		} else {
		    LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		testStepNumber = "s14";
		status = true;
		errorMessage = "Unable to verify the presence of all three backup files under /opt";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 14: DESCRIPTION: Verify that the three backup files are created once again after restarting securemount service");
		LOGGER.info(
			"STEP 14: ACTION: Execute command:: if [ -f /opt/<filename> ] ; then echo \"true\" ; else echo \"false\" ; fi");
		LOGGER.info("STEP 14: EXPECTED: All the three backup files should be present under /opt");
		LOGGER.info("**********************************************************************************");
		result = BroadBandSystemUtils.verifyBackupFilesArePresentForEncryption(device, tapEnv);
		errorMessage = result.getErrorMessage();
		status = result.isStatus();
		if (status) {
		    LOGGER.info(
			    "STEP 14: ACTUAL : Successfully verified the presence of all three backup files under /opt");
		} else {
		    LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		testStepNumber = "s15";
		status = false;
		errorMessage = "Standard encryption key is not used for encryption";
		LOGGER.info("**********************************************************************************");
		LOGGER.info(
			"STEP 15 : DESCRIPTION: Verify the logs 'Mounted cpc directory succssfully' in case of successful mounting of /opt/secure");
		LOGGER.info(
			"STEP 15 : ACTION: Execute command:: grep -i \"Mounted cpc directory succssfully\" /opt/logs/ecfs.txt");
		LOGGER.info("STEP 15 : EXPECTED: Expected logs should be present");
		LOGGER.info("**********************************************************************************");
		response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			BroadBandTraceConstants.LOG_TO_VERIFY_OPT_SECURE_IS_MOUNTED,
			BroadBandCommandConstants.FILE_PATH_ECFS_TXT, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(response);
		if (status) {
		    LOGGER.info(
			    "STEP 15: ACTUAL : Successfully verified the logs suggesting successful mount of /opt/secure folder");
		} else {
		    LOGGER.error("STEP 15: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");
		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

		if (!(isFibreDevice)) {
		    testStepNumber = "s16";
		    status = false;
		    errorMessage = "Unable to ummount /opt/secure folder";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info(
			    "STEP 16 : DESCRIPTION: Restart securemount service when /opt/secure is already mounted");
		    LOGGER.info("STEP 16 : ACTION: Execute command:: systemctl restart securemount");
		    LOGGER.info("STEP 16 : EXPECTED: securemount service should be restarted successfully");
		    LOGGER.info("**********************************************************************************");

		    status = CommonUtils.restartServiceAndVerifyServiceStatus(device, tapEnv,
			    BroadBandTestConstants.SECUREMOUNT_SERVICE, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);

		    if (status) {
			LOGGER.info("STEP 16: ACTUAL : Successfully restarted securemount service");
		    } else {
			LOGGER.error("STEP 16: ACTUAL : " + errorMessage);
		    }
		    LOGGER.info("**********************************************************************************");
		    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

		    testStepNumber = "s17";
		    status = false;
		    errorMessage = "Unable to verify whether log line 'Failed to mount cpc directory' is present in /opt/logs/ecfs.txt or not";
		    LOGGER.info("**********************************************************************************");
		    LOGGER.info(
			    "STEP 17 : DESCRIPTION: Verify that error log 'Failed to mount cpc directory' is observed on restarting securemount service when /opt/secure is already mounted");
		    LOGGER.info(
			    "STEP 17 : ACTION: Execute command:: grep -i \"Failed to mount cpc directory\" /opt/logs/ecfs.txt");
		    LOGGER.info("STEP 17 : EXPECTED: Expected log line should be present");
		    LOGGER.info("**********************************************************************************");
		    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			    BroadBandTraceConstants.LOG_TO_VERIFY_OPT_SECURE_IS_NOT_MOUNTED,
			    BroadBandCommandConstants.FILE_PATH_ECFS_TXT, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

		    status = CommonMethods.isNotNull(response);

		    if (status) {
			LOGGER.info(
				"STEP 17: ACTUAL : Successfully verified that error log is observed on restarting securemount service when /opt/secure is already mounted");
		    } else {
			LOGGER.error("STEP 17: ACTUAL : " + errorMessage);
		    }
		    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
		} else {
		    int stepNumber = 16;
		    while (stepNumber < BroadBandTestConstants.CONSTANT_18) {
			errorMessage = "Steps are not applicable for Fiber Devices Devices";
			testStepNumber = "S" + stepNumber;
			LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
			LOGGER.info(
				"**********************************************************************************");
			tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber,
				ExecutionStatus.NOT_APPLICABLE, errorMessage, false);
			stepNumber++;
		    }
		}
	    } else {
		LOGGER.info(
			"STEP 10-17:  These Steps are not applicable for Atom Console Devices, Since the feature is available only for ARM side");
		for (int loopCounter = BroadBandTestConstants.CONSTANT_10; loopCounter < BroadBandTestConstants.CONSTANT_18; loopCounter++) {
		    testStepNumber = "s" + Integer.toString(loopCounter);
		    tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			    errorMessage, false);
		}
	    }
	    LOGGER.info("**********************************************************************************");

	    testStepNumber = "s18";
	    status = false;
	    errorMessage = "response is not null after executing command umount /opt/secure which implies /opt/secure folder is not unmounted successfully";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 18: DESCRIPTION: Unmount /opt/secure once again");
	    LOGGER.info("STEP 18: ACTION: Execute command :: umount /opt/secure");
	    LOGGER.info("STEP 18: Expected: /opt/secure folder should be unmounted without any issue");
	    LOGGER.info("**********************************************************************************");
	    // executing command to unmount /opt/secure folder
	    response = tapEnv.executeCommandUsingSsh(device,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_TO_UNMOUNT_FILE_SYSTEM,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER.substring(BroadBandTestConstants.CONSTANT_0,
				    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER.length()
					    - BroadBandTestConstants.CONSTANT_1)));
	    status = CommonMethods.isNull(response) || (CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.TEXT_INVALID_ARGUMENT)
		    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTraceConstants.LOG_TARGET_IS_BUSY));
	    if (status) {
		LOGGER.info("STEP 18: ACTUAL : Successfully unmounted /opt/secure folder");
	    } else {
		LOGGER.error("STEP 18: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s19";
	    status = false;
	    errorMessage = "Unable to remove all three backup files under /opt";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 19: DESCRIPTION: Remove all the three backup files under /opt");
	    LOGGER.info(
		    "STEP 19: ACTION: Execute command:: 1) rm -rf /opt/filename 2) rm -rf /opt/filename 3) rm -rf /opt/filename");
	    LOGGER.info("STEP 19: EXPECTED: File removal should be successful");
	    LOGGER.info("**********************************************************************************");
	    result = BroadBandSystemUtils.removeBackupFilesUsedForEncryption(device, tapEnv);
	    errorMessage = result.getErrorMessage();
	    status = result.isStatus();
	    if (status) {
		LOGGER.info("STEP 19: ACTUAL : Successfully removed all three backup files under /opt");
	    } else {
		LOGGER.error("STEP 19: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s20";
	    status = false;
	    errorMessage = "Device could not be rebooted successfully";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 20: DESCRIPTION: Reboot the device to mount secure folder");
	    LOGGER.info("STEP 20: ACTION: Execute command:: reboot");
	    LOGGER.info("STEP 20: EXPECTED: Device should be rebooted successfully");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP 20: ACTUAL : Successfully rebooted the device");
	    } else {
		LOGGER.error("STEP 20: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s21";
	    status = false;
	    errorMessage = "Old /opt/secure content (sample.txt file) is still available";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 21: DESCRIPTION: Verify that previous contents of /opt/secure are no longer available.");
	    LOGGER.info(
		    "STEP 21: ACTION: Execute command:: if [ -f /opt/secure/sample.txt ] ; then echo \"true\" ; else echo \"false\" ; fi");
	    LOGGER.info(
		    "STEP 21: EXPECTED: sample.txt file should not be present as all the backup files used to create encryption keys were deleted and new key got created upon restarting securemount");
	    LOGGER.info("**********************************************************************************");
	    status = !CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_PATH_SECURE_FOLDER
		    + BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY);
	    if (status) {
		LOGGER.info(
			"STEP 21: ACTUAL : Successfully verified that old /opt/secure content is no longer available");
	    } else {
		LOGGER.error("STEP 21: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error("Exception caught during execution !!!! " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, testStepNumber, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info(
		    "POST-CONDITION : DESCRIPTION : Verify that sample.txt does not exist and remove it if it exists");
	    LOGGER.info(
		    "POST-CONDITION : ACTION : 1) rm -rf /opt/secure/sample.txt 2) rm -rf /opt/secure/ECRYPTFS_FNEK_ENCRYPTED*");
	    LOGGER.info("POST-CONDITION : EXPECTED : Sample.txt file should be removed");
	    // executing command to remove sample.txt from /opt/secure folder
	    tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_DIR_FORCEFULLY,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER,
			    BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY));
	    tapEnv.executeCommandUsingSsh(device,
		    CommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_REMOVE_DIR_FORCEFULLY,
			    BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.FILE_PATH_SECURE_FOLDER,
			    BroadBandTestConstants.ENCRYPTED_FILE_STRING + BroadBandTestConstants.ASTERISK));
	    status = !CommonUtils.isFileExists(device, tapEnv, BroadBandTestConstants.FILE_PATH_SECURE_FOLDER
		    + BroadBandTestConstants.FILE_CREATED_TO_TEST_SECURITY);
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-3042");
    }

    /**
     * 
     * Test Case : Verify Add IP from ARM/ATOM side private network using Ping and ip route
     * 
     * <ol>
     * <li>Step 1: Verify ping 8.8.8.8 response in ARM/ATOM console</li>
     * <li>Step 2: Verify dev interface default ip in ip route response at ARM/ATOM console</li>
     * <li>Step 3: Verify dev interface proto kernel scope link in ip route response at ARM/ATOM console</li>
     * <li>Step 4: Verify dev interface scope link in ip route response at ARM/ATOM console</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Muthukumar
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-PING-IPRUT-5001")
    public void testToVerifyPingAndIpRouteResponse(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-PING-IPRUT-501";
	int stepNumber = 1;
	String errorMessage = null;
	String step = "S" + stepNumber;
	boolean status = false;
	String response = null;
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-PING-IPRUT-5001");
	    LOGGER.info("TEST DESCRIPTION: Verify Add IP from ATOM side private network using Ping and ip route");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("Step 1: Verify ping 8.8.8.8 response in ARM/ATOM console");
	    LOGGER.info("Step 2: Verify dev interface default ip  in ip route response at ARM/ATOM console");
	    LOGGER.info(
		    "Step 3: Verify dev interface proto kernel scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("Step 4: Verify dev interface scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1 : Verify ping 8.8.8.8 response in ARM/ATOM console
	     */
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify ping 8.8.8.8 response in ARM/ATOM console");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command ping -c 5 8.8.8.8 ");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Ping response should get successful");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify the ping 8.8.8.8 response in ARM/ATOM console";
	    response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommandConstants.CMD_PING_8_8_8_8);
	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
		    BroadBandConnectedClientTestConstants.PATTERN_MATCHER_PING_RESPONSE_LINUX);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified ping 8.8.8.8 response in ARM/ATOM console");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * STEP 2 : Verify dev interface default ip in ip route response at ARM/ATOM console
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify dev interface default ip in ip route response at ARM/ATOM console");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command ip route ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Must return the dev interface default ip in ip route response at ARM/ATOM console");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify dev interface default ip in ip route response at ARM/ATOM console";
	    response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommandConstants.CMD_SBIN_IP_ROUTE);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_DEFAULT_DEV_IP);
	    if (!status) {
		response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
			BroadBandCommandConstants.CMD_IP_ROUTE);
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_DEFAULT_DEV_IP);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified dev interface default ip in ip route response at ARM/ATOM console");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * STEP 3 : Verify dev interface proto kernel scope link in ip route response at ARM/ATOM console
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify dev interface proto kernel scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command ip route ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Must return dev interface proto kernel scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify dev interface proto kernel scope link in ip route response at ARM/ATOM console";
	    response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommandConstants.CMD_SBIN_IP_ROUTE);
	    status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_DEV_PROTO_KERNEL_SCOPE_LINK);
	    if (!status) {
		response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
			BroadBandCommandConstants.CMD_IP_ROUTE);
		status = CommonMethods.isNotNull(response) && CommonMethods.patternMatcher(response,
			BroadBandTestConstants.PATTERN_DEV_PROTO_KERNEL_SCOPE_LINK);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified dev interface proto kernel scope link in ip route response at ARM/ATOM console");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * STEP 4 : Verify dev interface scope link in ip route response at ARM/ATOM console
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    errorMessage = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify dev interface scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute command ip route ");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Must return dev interface scope link in ip route response at ARM/ATOM console");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to Verify dev interface scope link in ip route response at ARM/ATOM console";
	    response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommandConstants.CMD_SBIN_IP_ROUTE);
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_DEV_SCOPE_LINK);
	    if (!status) {
		response = BroadBandCommonUtils.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
			BroadBandCommandConstants.CMD_IP_ROUTE);
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_DEV_SCOPE_LINK);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Successfully verified dev interface scope link in ip route response at ARM/ATOM console");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-PING-IPRUT-5001");
    }

    /**
     *
     * Test Case : Verify fixes for ccsp-webpa-adapter N ,WiFi values,2.4 & 5 GHz Radios and common parameter after
     * webpa reboot
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Pre Condition 1 : Retrieve value for 2.4 & 5 ghz radios from device</li>
     * <li>Pre Condition 2 : Retrieve common parameter values from device</li>
     * <li>Step 1 : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep
     * webpa</li>
     * <li>Step 2 : Verify the SSID value using the WebPA param Device.WiFi.SSID.10001.SSID</li>
     * <li>Step 3 : Verify the CM MAC is retrieved using the WEBPA Param: Device.DeviceInfo.X_COMCAST-COM_CM_MAC</li>
     * <li>Step 4 : Retrieve WAN IP and Client IP Router values from device</li>
     * <li>Step 5 : Set and verify the value for 2.4 & 5 GHz Radios in device</li>
     * <li>Step 6 : Set and verify the common parameter values in device</li>
     * <li>Step 7 : Retrieve value for 2.4 & 5 GHz Radios from device</li>
     * <li>Step 8 : Retrieve common parameter values from device</li>
     * <li>Step 9 : Enable Bridge mode in device</li>
     * <li>Step 10 : Verify the device can be rebooted using the using webpa
     * param:Device.X_CISCO_COM_DeviceControl.RebootDevice</li>
     * <li>Step 11 : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep
     * webpa</li>
     * <li>Step 12 : Verify last reboot reason value when reboot triggered through WebPA using the following param
     * Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason</li>
     * <li>Step 13 : Verify the SSID value using the WebPA param Device.WiFi.SSID.10001.SSID</li>
     * <li>Step 14 : Verify WebPA get for Device.IP.Interface.1.Enable with Value Either True fro Enable and False for
     * Disable.</li>
     * <li>Step 15 : Retrieve value for 2.4 & 5 GHz Radios from device and cross verify with values retrieved from Step
     * #7</li>
     * <li>Step 16 : Retrieve common parameter values from device and cross verify with values retrieved from Step
     * #8</li>
     * <li>Step 17 : Retrieve WAN IP and Client IP Router values from from Step #4</li>
     * <li>Post Condition 1 : Disable Bridge Mode</li>
     * <li>Post Condition 2 : Set value for 2.4 & 5 ghz radios from device</li>
     * <li>Post Condition 3 : Set the initial common parameter values</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Manikandan, Muthukumar
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-9010")
    public void testToVerifyFixesForCCSPWebpaAdapter(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SYSTEM-910";
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	String testStepNumber = "S" + stepNumber;
	String errorMessage = null;
	boolean status = false;
	long startTime = 0L;
	boolean isFibreDevice = false;
	String ssidObtainedViaWebpa = null;
	String ssidObtainedViaSNMP = null;
	String cmMacViaWebpa = null;
	boolean isAtomSyncAvailable = false;
	Map<String, String> initialWiFiRadioValues = null;
	Map<String, String> initialCommonParamValues = null;
	int postConStepNumber = BroadBandTestConstants.CONSTANT_1;
	int preCondNumber = BroadBandTestConstants.CONSTANT_1;
	Map<String, String> wiFiRadioValues = null;
	Map<String, String> commonParamValues = null;
	Map<String, String> initialWanIpAndClientIPValues = null;
	boolean isBridgeModeEnabled = false;
	// Variable Declaration Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-9010");
	LOGGER.info("TEST DESCRIPTION: Verify fixes for ccsp-webpa-adapter");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Pre Condition 1 : Retrieve value for 2.4 & 5 ghz radios from device");
	LOGGER.info("Pre Condition 2 : Retrieve common parameter values from device");
	LOGGER.info(
		"Step 1 : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep webpa");
	LOGGER.info("Step 2 : Verify the SSID value using the WebPA param Device.WiFi.SSID.10001.SSID");
	LOGGER.info(
		"Step 3 : Verify the CM MAC is retrieved using the WEBPA Param: Device.DeviceInfo.X_COMCAST-COM_CM_MAC");
	LOGGER.info("Step 4 : Retrieve WAN IP and Client IP Router values from device");
	LOGGER.info("Step 5 : Set and verify the value for 2.4 & 5 GHz Radios in device");
	LOGGER.info("Step 6 : Set and verify the common parameter values in device");
	LOGGER.info("Step 7 : Retrieve value for 2.4 & 5 GHz Radios from device");
	LOGGER.info("Step 8 : Retrieve common parameter values from device");
	LOGGER.info("Step 9 : Enable Bridge mode in device");
	LOGGER.info(
		"Step 10 : Verify the device can be rebooted using the using webpa  param:Device.X_CISCO_COM_DeviceControl.RebootDevice");
	LOGGER.info(
		"Step 11 : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep  webpa");
	LOGGER.info(
		"Step 12 : Verify last reboot reason value when reboot triggered through WebPA using the following param Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	LOGGER.info("Step 13 : Verify the SSID value using the WebPA param Device.WiFi.SSID.10001.SSID");
	LOGGER.info(
		"Step 14 : Verify WebPA get for Device.IP.Interface.1.Enable with Value Either True fro Enable and False for Disable.");
	LOGGER.info(
		"Step 15 : Retrieve value for 2.4 & 5 GHz Radios from device and cross verify with values retrieved from Step #7");
	LOGGER.info(
		"Step 16 : Retrieve common parameter values from device and cross verify with values retrieved from Step #8");
	LOGGER.info("Step 17 : Retrieve WAN IP and Client IP Router values from from Step #4");
	LOGGER.info("Post Condition 1 : Disable Bridge Mode");
	LOGGER.info("Post Condition 2 : Set value for 2.4 & 5 ghz radios from device");
	LOGGER.info("Post Condition 3 : Set the initial common parameter values");
	LOGGER.info("#######################################################################################");
	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    /**
	     * PRE-CONDITION 1 : RETRIEVE VALUE FOR 2.4 & 5 GHZ RADIOS FROM DEVICE
	     */
	    preCondNumber++;
	    initialWiFiRadioValues = BroadBandPreConditionUtils
		    .executePreConditionToGetTheDefaultValuesForBothRadios(device, tapEnv, preCondNumber);

	    /**
	     * PRE-CONDITION 2 : RETRIEVE COMMON PARAMETER VALUES FROM DEVICE
	     */
	    preCondNumber++;
	    initialCommonParamValues = BroadBandPreConditionUtils
		    .executePreConditionToGetTheDefaultValuesForCommonParams(device, tapEnv, preCondNumber);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * Step 1 : VERIFY WEBPA PROCESS IS RUNNING ON ATOM SIDE USING THE FOLLOWING COMMAND IN ATOM CONSOLE
	     */
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Webpa Process is not up and Running and is not  Listed in usr/bin/ File.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep webpa");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Exeute the command to check whether webpa is running or not ps | grep webpa");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Webpa Process Should be up and Running and get Listed in (/usr/bin/webpa) File.");
	    LOGGER.info("**********************************************************************************");
	    startTime = System.currentTimeMillis();
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    do {
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, isAtomSyncAvailable,
			BroadBandTestConstants.PROCESS_NAME_WEBPA);
	    } while (!status
		    && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Webpa Process is up and Running and get Listed in (/usr/bin/webpa) as expected");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2 : VERIFY THE SSID VALUE USING THE WEBPA PARAM
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Unable to fetch the SSID of the device via WebPA param :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify the  SSID value using the WebPA param "
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Exeute the WebPA command :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : SSID value  should be obtained via webpa.");
	    LOGGER.info("**********************************************************************************");
	    isFibreDevice = DeviceModeHandler.isFibreDevice(device);
	    if (!isFibreDevice) {
		ssidObtainedViaSNMP = BroadBandSnmpUtils.executeSnmpGetWithTableIndexOnRdkDevices(tapEnv, device,
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getOid(),
			BroadBandSnmpMib.ECM_PRIVATE_WIFI_SSID_2_4.getTableIndex());
		status = CommonMethods.isNotNull(ssidObtainedViaSNMP)
			&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID, ssidObtainedViaSNMP,
				BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } else {
		ssidObtainedViaWebpa = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
		status = CommonMethods.isNotNull(ssidObtainedViaWebpa);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : SSID value  has been obtained via webpa as expected.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 3 : VERIFY THE CM MAC IS RETRIEVED USING THE WEBPA PARAM
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Unable to fetch the CM MAC via WEBPA using " + BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify the  CM MAC  is retieved using the WEBPA Param:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Exeute the webpa command :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC);
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Response should return CM MAC received from WEBPA API request");
	    LOGGER.info("**********************************************************************************");
	    cmMacViaWebpa = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_CM_MAC);
	    status = CommonMethods.isNotNull(cmMacViaWebpa) && CommonMethods.isMacValid(cmMacViaWebpa);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : CM_MAC of the device is obtained from WEBPA GET API request as expected");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 4 : RETRIEVE WAN IP AND CLIENT IP ROUTER VALUES FROM DEVICE
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    initialWanIpAndClientIPValues = BroadBandFactoryResetTests
		    .executeTestSeptsToGetAndVerifyWanIPAndClientIP(device, testCaseId, stepNumber, null, false);

	    /**
	     * Step 5 : SET AND VERIFY THE NON DEFAULT VALUES FOR RADIOS
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    wiFiRadioValues = BroadBandFactoryResetTests.executeTestSeptsToSetTheRadioValues(device, testCaseId,
		    stepNumber);

	    /**
	     * Step 6 : Set and verify the common parameter values in device
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    commonParamValues = BroadBandFactoryResetTests.executeTestSeptsToSetTheCommonParameterValues(device,
		    testCaseId, stepNumber);

	    /**
	     * Step 7 : RETRIEVE VALUES FOR 2.4 & 5 GHZ RADIOS FROM DEVICE AFTER SET
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    BroadBandFactoryResetTests.executeTestSeptsToGetAndVerifyRadioValues(device, testCaseId, stepNumber,
		    wiFiRadioValues, true);

	    /**
	     * Step 8 : RETRIEVE COMMON PARAMETER VALUES FROM DEVICE AFTER SET
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    BroadBandFactoryResetTests.executeTestSeptsToGetAndVerifyCommonParameterValues(device, testCaseId,
		    stepNumber, commonParamValues, true);

	    /**
	     * Step 9 : ENABLE BRIDGE MODE IN DEVICE
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    isBridgeModeEnabled = BroadBandFactoryResetTests.executeTestSeptsToEnableBridgeMode(device, testCaseId,
		    stepNumber);

	    /**
	     * Step 10 : VERIFY THE DEVICE CAN BE REBOOTED USING WEBPA
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Unable to reboot the device using webpa";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify the device can be rebooted using webpa param: Device.X_CISCO_COM_DeviceControl.RebootDevice");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Exeute the WebPA command :Device.X_CISCO_COM_DeviceControl.RebootDevice");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Device should go for a reboot and come online after the reboot .");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandCommonUtils.rebootViaWebpaAndWaitForStbAccessible(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Device has gone for a reboot and come online after the reboot");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 11 : VERIFY WEBPA PROCESS IS RUNNING ON ATOM SIDE
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Webpa Process is not up and Running and is not  Listed in usr/bin/ File.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify webpa process is running on ATOM side using the following command in ATOM console: ps | grep webpa");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Exeute the command to check whether webpa is running or not ps | grep webpa");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Webpa Process Should be up and Running and get Listed in (/usr/bin/webpa) File.");
	    LOGGER.info("**********************************************************************************");
	    startTime = System.currentTimeMillis();
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    do {
		status = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, isAtomSyncAvailable,
			BroadBandTestConstants.PROCESS_NAME_WEBPA);
	    } while (!status
		    && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Webpa Process is up and Running and get Listed in (/usr/bin/webpa) as expected");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 12 : VERIFY LAST REBOOT REASON VALUE WHEN REBOOT TRIGGERED
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Last Reboot Reason obtianed is not  \"webpa-reboot\"  as Expected.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify last reboot reason value when reboot triggered through WebPA  using the following param :"
		    + BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Exeute the command:"
		    + BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT);
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : "
		    + BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT
		    + " value should be Obatined for Last Reboot Reason using WEBPA Param : "
		    + BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true)
		    && BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON,
			    BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT,
			    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL :"
			+ BroadBandTelemetryConstants.LAST_REBOOT_REASON_FOR_WEBPA_REBOOT
			+ "value has been Obatined for Last Reboot Reason using WEBPA Param :"
			+ BroadBandWebPaConstants.WEBPA_COMMAND_LAST_REBOOT_REASON);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 13 : VERIFY THE SSID VALUE
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "SSID value obtained via webpa is not same as the Value Obtained in Step 2.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify the  SSID value using the WebPA param"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Exeute the webpa command :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID);
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : SSID value obtained via webpa should be as same as the Value Obtained in Step 2.(Persist)");
	    LOGGER.info("**********************************************************************************");
	    if (!isFibreDevice) {
		status = CommonMethods.isNotNull(ssidObtainedViaSNMP)
			&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID, ssidObtainedViaSNMP,
				BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } else {
		status = CommonMethods.isNotNull(ssidObtainedViaWebpa)
			&& BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
				BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_SSID_10001_SSID, ssidObtainedViaWebpa,
				BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SSID value obtained via webpa is as same as the Value Obtained in Step 2 before WEBPA Reboot.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 14 : VERIFY THE WEBPARAM INTERFACE ENABLE VALUE
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    errorMessage = "Value Obatained is not either True For Enabled and False for Disabled State as Expected.";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify WebPA get for "
		    + BroadBandWebPaConstants.WEBPA_PARAM_INTERFACE_ENABLE
		    + " with Value Either True fro Enable and False for Disable.");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Exeute the WebPA command :"
		    + BroadBandWebPaConstants.WEBPA_PARAM_INTERFACE_ENABLE);
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Value Obatained should be Either True For Enabled and False for Disabled State.");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_INTERFACE_ENABLE, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS)
		    || BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_INTERFACE_ENABLE, BroadBandTestConstants.FALSE,
			    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : Value Obatained is in either True For Enabled and False for Disabled State.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 15 : RETRIEVE AND VERIFY THE VALUE FOR 2.4 & 5 GHZ RADIOS FROM DEVICE AFTER REBOOT
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    BroadBandFactoryResetTests.executeTestSeptsToGetAndVerifyRadioValues(device, testStepNumber, stepNumber,
		    wiFiRadioValues, true);

	    /**
	     * Step 16 : RETRIEVE AND VERIFY THE COMMON PARAMETERS FROM DEVICE AFTER REBOOT
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    BroadBandFactoryResetTests.executeTestSeptsToGetAndVerifyCommonParameterValues(device, testStepNumber,
		    stepNumber, commonParamValues, true);

	    /**
	     * Step 17 : RETRIEVE WAN IP AND CLIENT IP ROUTER VALUES FROM DEVICE AFTER REBOOT
	     */
	    stepNumber++;
	    testStepNumber = "S" + stepNumber;
	    BroadBandFactoryResetTests.executeTestSeptsToGetAndVerifyWanIPAndClientIP(device, testStepNumber,
		    stepNumber, initialWanIpAndClientIPValues, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

	    /**
	     * POST-CONDITION 1 : DISABLE BRIDGE MODE
	     */
	    if (isBridgeModeEnabled) {
		BroadBandPostConditionUtils.executePostConditionToDisableBirdgeMode(device, tapEnv, postConStepNumber);
		postConStepNumber++;
	    }

	    /**
	     * POST CONDITION 2 : POST-CONDITION METHOD TO SET THE WIFI RADIO INITIAL VALUES
	     */
	    BroadBandPostConditionUtils.executePostConditionToSetTheDefaultValuesForBothRadios(device, tapEnv,
		    postConStepNumber, initialWiFiRadioValues);
	    postConStepNumber++;

	    /**
	     * POST CONDITION 2 : POST-CONDITION METHOD TO TO SET THE COMMON PARAM VALUES
	     */
	    BroadBandPostConditionUtils.executePostConditionToSetTheDefaultValuesForCommonParams(device, tapEnv,
		    postConStepNumber, initialCommonParamValues);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-9010");
    }

    /**
     * This test case is to Verify CPU usage verify when CPU usage stays 100% for over 5 mins, top 5 processes gets
     * logged in SelfHeal.txt.0
     * 
     * <ol>
     * <li>STEP 1 : Make CPU usage as 100% using system commands</li>
     * <li>STEP 2 : Verify CPU usage 100% reached status</li>
     * <li>STEP 3 : Verify Avg CPU usage after 5 minutes of CPU Avg monitor window is 100</li>
     * <li>STEP 4 : Verify when CPU usage stays 100% for over 5 mins, top 5 processes gets logged in SelfHeal.txt.0</li>
     * <li>STEP 5 : Verify log format logged in SelfHeal.txt.0</li>
     * </ol>
     * 
     * @param device
     *            Dut to be used
     * @author Gnanaprakasham S
     * @refactor Alan_Bivera
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1028")
    public void verifyProcessDetailsWhenCpuUsageHundredPercentage(Dut device) {

	// Variable to store test case ID
	String testCaseId = "TC-RDKB-SYSTEM-028";
	// Variable to store test step number
	String testStepNumber = "s1";
	// Variable to store execution status
	boolean status = false;
	// Variable to store error Message
	String errorMessage = null;
	String response = null;

	try {
	    LOGGER.info("*************************************************************************************");
	    LOGGER.info("STARTING TEST CASE: " + testCaseId);
	    LOGGER.info(
		    "TEST DESCRIPTION: verify when CPU usage stays 100% for over 5 mins, top 5 processes gets logged in SelfHeal.txt.0");
	    LOGGER.info("***************************************************************************************");

	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Make CPU usage as 100% using system commands");
	    LOGGER.info("2. Verify CPU usage 100% reached status");
	    LOGGER.info("3. Verify Avg CPU usage after 5 minutes of CPU Avg monitor window is 100");
	    LOGGER.info(
		    "4. Verify when CPU usage stays 100% for over 5 mins, top 5 processes gets logged in SelfHeal.txt.0 ");
	    LOGGER.info("5. Verify log format logged in SelfHeal.txt.0 ");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Make CPU usage as 100% using system commands ");
	    LOGGER.info("STEP 1: ACTION : Execute the command 10 times -  yes > /dev/null &");
	    LOGGER.info("STEP 1: EXPECTED : command execution must be successful ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to execute \"yes > /dev/null &\" command to make ";
	    if (DeviceModeHandler.isFibreDevice(device)) {
		for (int iteration = BroadBandTestConstants.CONSTANT_0; iteration <= BroadBandTestConstants.CONSTANT_9; iteration++) {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.COMMAND_TO_MAKE_CPU_USAGE_HUNDRED_PERCENT);
		    status = CommonMethods.isNotNull(response);
		}
	    } else {
		for (int iteration = BroadBandTestConstants.CONSTANT_0; iteration <= BroadBandTestConstants.CONSTANT_9; iteration++) {
		    response = tapEnv.executeCommandUsingSsh(device,
			    BroadBandTestConstants.COMMAND_TO_MAKE_CPU_USAGE_HUNDRED_PERCENT);
		    status = CommonMethods.isNull(response);
		}
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Making CPU usage as 100% is successful");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify CPU usage 100% reached status");
	    LOGGER.info("STEP 2: ACTION : Execute command - grep -i \"CPU usage is 100\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP 2: EXPECTED : CPU usage must be reached 100% ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to get CPU usage 100% logs in SelfHeal.txt.0 file";
	    status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_CPU_USAGE_100_PERCENT, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
		    BroadBandTestConstants.TWENTYFIVE_MINUTES_IN_MILLS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully obtained CPU usage 100% logs in selfheal.txt file");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Avg CPU usage after 5 minutes of CPU Avg monitor window is 100");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute command - grep -i \"RDKB_SELFHEAL : Avg CPU usage after 5 minutes of CPU Avg monitor window is 100\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP 3: EXPECTED : Avg CPU usage after 5 minutes of CPU Avg monitor window should be 100 ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to get Avg CPU usage after 5 minutes of CPU Avg monitor window as 100 in SelfHeal.txt.0 log file";
	    status = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.STRING_AVG_CPU_AVG_MONITOR_WINDOW,
		    BroadBandCommandConstants.LOG_FILE_SELFHEAL, BroadBandTestConstants.TWENTYFIVE_MINUTES_IN_MILLS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : Successfully obtained Avg CPU usage after 5 minutes of CPU Avg monitor window is 100 in selfheal.txt file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    testStepNumber = "s4";
	    status = false;
	    boolean isTopFiveProcessLogPresent = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify when CPU usage stays 100% for over 5 mins, top 5 processes gets logged in SelfHeal.txt.0 ");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command - grep -i \"RDKB_SELFHEAL : Top 5 tasks running on device\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP 4: EXPECTED : Top 5 processes are logged in SelfHeal.txt.0  ");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify top 5 processes details logged in SelfHeal.txt.0 file when CPU usage reaches 100 %";
	    isTopFiveProcessLogPresent = CommonUtils.isNotEmptyOrNull(BroadBandCommonUtils.searchLogFiles(tapEnv,
		    device, BroadBandTestConstants.STRING_TOP_FIVE_TASK, BroadBandCommandConstants.LOG_FILE_SELFHEAL,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (isTopFiveProcessLogPresent) {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandCommandConstants.LOG_FILE_SELFHEAL));
		if (CommonMethods.isNotNull(response)) {
		    // Verify process details from selfheal.txt file
		    status = verifyTopFiveProcessLoggedStatus(response);
		} else {
		    errorMessage = "Obatained null response from SelfHeal.txt.0";
		    LOGGER.error(errorMessage);
		}
	    } else {
		errorMessage = "Top 5 tasks running on device logs are not present in SelfHeal.txt.0";
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL : Verified  CPU usage stays 100% for over 5 mins and top 5 processes gets logged in SelfHeal.txt.0");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify log format logged in SelfHeal.txt.0");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute command - grep -i \"RDKB_SELFHEAL : CPU load at 100\" /rdklogs/logs/SelfHeal.txt.0");
	    LOGGER.info("STEP 5: EXPECTED : Log message should be correct format ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to verify log format in SelfHeal.txt.0";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_100_CPU_LOAD_FROM_SELFHEAL,
		    BroadBandCommandConstants.LOG_FILE_SELFHEAL);
	    status = CommonMethods.patternMatcher(response,
		    BroadBandTestConstants.PATTERN_TO_VALIDATE_MSG_FROMAT_FROM_SELFHEAL);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfully Verified log format logged in SelfHeal.txt.0");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = "Exception occurred during execution : " + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, false, errorMessage,
		    true);
	} finally {
	    // kill all the generated process
	    tapEnv.executeCommandUsingSsh(device, "killall yes");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    String pid = CommonMethods.getPidOfProcess(device, tapEnv, "yes");
	    status = CommonMethods.isNull(pid);
	    if (status) {
		LOGGER.info(
			"POSTCONDITION : ACTUAL : The process \"yes\" generated suring this test has been killed .");

	    } else {
		LOGGER.error(
			"POSTCONDITION: ACTUAL : The process \"yes\" generated suring this test has not been killed .");
	    }
	}
	LOGGER.info("ENDING TEST CASE : " + testCaseId);
    }

    /**
     * Helper method to get top 5 process details when CPU usage reaches 100 %
     * 
     * @param response
     *            - Logs obtained in SelfHeal.txt file
     * 
     * @return true, if expected process details present.
     * 
     * @author gnanaprakasham.s
     * @refactor Alan_Bivera
     */
    private boolean verifyTopFiveProcessLoggedStatus(String response) {
	LOGGER.debug("STARTING METHOD : verifyTopFiveProcessLoggedStatus()");
	boolean status = false;
	List<String> process = new ArrayList<String>();
	try {
	    process = CommonMethods.patternFinderToReturnAllMatchedString(response,
		    BroadBandTestConstants.REGEX_TOP_FIVE_PROCESS);
	    LOGGER.info("Obtained top 5 Process Details is: " + process);
	    LOGGER.info("Obtained process count : " + process.size());
	    status = !process.isEmpty() && process.size() >= BroadBandTestConstants.CONSTANT_5;
	    LOGGER.debug("ENDING METHOD : verifyTopFiveProcessLoggedStatus()");
	} catch (Exception e) {
	    LOGGER.error("Exception occurred in verifyTopFiveProcessLoggedStatus. Error -" + e.getMessage());
	}
	return status;
    }

    /**
     *
     * Test Case : Verify Webpa Notifications on Firmware Upgrade.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Perform factory reset on a device.</li>
     * <li>PRE-CONDITION 2 : Reactivate the Device.</li>
     * <li>Step 1 : Verify parodus process is running on ARM side.</li>
     * <li>Step 2 : Verify Triggered CDL Successful with latest firmware version.</li>
     * <li>Step 3 : Verify old firmware version is available in WEBPAlog.txt.0 log.</li>
     * <li>Step 4 : Verify current firmware version is available in WEBPAlog.txt.0 log.</li>
     * <li>Step 5 : Verify whether webpa request 'get' is communicating with the device after firmware upgrade.</li>
     * <li>Step 6 : Verify whether webpa request 'set' is communicating with the device after firmware upgrade.</li>
     * <li>Step 7 : Verify Triggered CDL Successful with previous firmware version.</li>
     * <li>POST-CONDITION 1 : Verify device is reactivated,if already not activated.</li>
     * <li>POST-CONDITION 2 : Verify that device is upgraded with the previous firmware version in #S7, if not try CDL
     * again.</li>
     * 
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * 
     * @author Elangovan
     * @refactor Rakesh C N, Sruthi Santhosh
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-5003")
    public void testToVerifyWebpaFirmwareUpgrade(Dut device) {
	String testCaseId = "TC-RDKB-SYSTEM-503";
	String errorMessage = null;
	boolean status = false;
	int stepNumber = 1;
	String step = "S" + stepNumber;
	boolean isFactoryResetDone = false;
	boolean isReactivated = false;
	int preConStepNumber = 1;
	int postConStepNumber = 1;
	String initialFirmwareVersion = null;
	boolean hasOriginalBuildChanged = false;
	boolean hasLatestBuildChanged = false;
	String webPAResponse = null;
	boolean result = false;

	String currentImageNameforPostCondition = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv,
		device);

	try {
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-5003");
	    LOGGER.info("TEST DESCRIPTION: Verify Webpa Notifications on Firmware Upgrade.");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(" PRE CONDITION 1 : Perform factory reset on a device.");
	    LOGGER.info(" PRE CONDITION 2 : Reactivate the Device.");
	    LOGGER.info("Step 1 : Verify parodus process is running on ARM side.");
	    LOGGER.info("Step 2 : Verify Triggered CDL Successful with latest firmware version.");
	    LOGGER.info("Step 3 : Verify old firmware version is available in WEBPAlog.txt.0 log.");
	    LOGGER.info("Step 4 : Verify current firmware version is available in WEBPAlog.txt.0 log");
	    LOGGER.info(
		    "Step 5 : Verify whether webpa request 'get' is communicating with the device after firmware upgrade.");
	    LOGGER.info(
		    "Step 6 : Verify whether webpa request 'set' is communicating with the device after firmware upgrade");
	    LOGGER.info("Step 7 : Verify Triggered CDL Successful with previous firmware version.");
	    LOGGER.info(" POST-CONDITION 1 : Verify device is reactivated,if already not activated.");
	    LOGGER.info(
		    " POST-CONDITION 2 :  Verify that device is upgraded with the previous firmware version in #S7, if not try CDL again.");
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : PERFORM FACTORY RESET ON THE DEVICE
	     */
	    isFactoryResetDone = BroadBandPreConditionUtils.executePreConditionToFactoryResetDevice(device, tapEnv,
		    preConStepNumber);
	    /**
	     * PRE-CONDITION 2 : REACTIVATE THE ROUTER DEVICE
	     */
	    preConStepNumber++;
	    isReactivated = BroadBandPreConditionUtils.executePreConditionToReacitivateDevice(device, tapEnv,
		    preConStepNumber);
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1: VERIFY PARODUS PROCESS IS RUNNING ON ARM SIDE
	     */
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber + " : DESCRIPTION : VERIFY PARODUS PROCESS IS RUNNING ON ARM SIDE.");
	    LOGGER.info("STEP :  " + stepNumber + " : ACTION : EXECUTE COMMAND: ps | grep parodus");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: PARODUS PROCESS DETAILS SHOULD BE DISPLAYED AS (/USR/BIN/PARODUS).");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "PARODUS PROCESS DETAILS IS NOT DISPLAYED";
	    String response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.COMMAND_TO_GET_PARODUS_PROCESS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.PARODUS_PROCESS_OUTPUT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : PARODUS PROCESS IS RUNNING ON ARM SIDE");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 2 : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFULL WITH LATEST FIRMWARE VERSION ");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : TRIGGER THE CDL WITH LATEST FIRMWARE VERSION USING TR-181/SNMP");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : CDL MUST SUCCESSFUL WITH LATEST FIRMWARE VERSION.");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH LATEST FIRMWARE VERSION.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    hasLatestBuildChanged = BroadBandCodeDownloadUtils.triggerLatestCodeDownload(device, tapEnv,
		    initialFirmwareVersion);
	    status = hasLatestBuildChanged;
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : CDL SUCCESSFUL WITH LATEST FIRMWARE VERSION");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /**
	     * Step 3 : VERIFY OLD FIRMWARE VERSION IS AVAILABLE IN WEBPALOG.TXT.0 LOG
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY OLD FIRMWARE VERSION IS AVAILABLE IN WEBPALOG.TXT.0 LOG");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND: grep -i 'oldFirmwareVer :' /rdklogs/logs/WEBPAlog.txt.0 ");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: LOG INFORMATION ABOUT OLD FIRMWARE VERSION SHOULD BE AVAILABLE IN WEBPALOG.TXT.0 LOG");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY THE OLD FIRMWARE VERSION IN WEBPALOG.TXT.0 LOG";
	    String patternMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_OLD_FIRMWARE,
		    BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(initialFirmwareVersion));
	    status = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcher(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_OLD_FIRMWARE, BroadBandCommandConstants.LOG_FILE_WEBPA,
		    patternMatcher, BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED THE LOG INFORMATION ABOUT OLD FIRMWARE VERSION IN WEBPALOG.TXT.0 LOG");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * SETP 4 : VERIFY CURRENT FIRMWARE VERSION IS AVAILABLE IN WEBPALOG.TXT.0 LOG
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY CURRENT FIRMWARE VERSION IS AVAILABLE IN WEBPALOG.TXT.0 LOG");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND: grep -i 'cur_firmware_ver value :' /rdklogs/logs/WEBPAlog.txt.0 ");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: LOG INFORMATION ABOUT CURRENT FIRMWARE VERSION SHOULD BE AVAILABLE IN WEBPALOG.TXT.0 LOG");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO VERIFY THE CURRENT FIRMWARE VERSION IN WEBPALOG.TXT.0 LOG";
	    patternMatcher = BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTraceConstants.LOG_MESSAGE_CURRENT_FIRMWARE,
		    BroadBandCommonUtils.removeDifferentSignedExtensionsInRequestedBuildName(
			    FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device)));
	    status = BroadBandCommonUtils.verifyLogsInAtomOrArmWithPatternMatcher(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_CURRENT_FIRMWARE, BroadBandCommandConstants.LOG_FILE_WEBPA,
		    patternMatcher, BroadBandTestConstants.BOOLEAN_VALUE_FALSE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED THE LOG INFORMATION ABOUT CURRENT FIRMWARE VERSION IN WEBPALOG.TXT.0 LOG");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * SETP 5 : VERIFY WHETHER WEBPA REQUEST 'GET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY WHETHER WEBPA REQUEST 'GET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND : Device.DeviceInfo.X_CISCO_COM_FirmwareName");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: THE DEVICE SHOULD RETURN THE VALUE FOR THE CORRESPONDING WEBPA PARAMETER.");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "FAILED TO GET THE VALUE OF THE WEBPA PARAMETER AFTER FIRMWARE UPGRADE";
	    webPAResponse = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAMETER_FOR_FIRMWARE_NAME);
	    status = CommonMethods.isNotNull(webPAResponse);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED WEBPA REQUEST 'GET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * SETP 6 : VERIFY WHETHER WEBPA REQUEST 'SET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : DESCRIPTION : VERIFY WHETHER WEBPA REQUEST 'SET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : ACTION : EXECUTE COMMAND : Device.WiFi.AccessPoint.10001.Security.ModeEnabled");
	    LOGGER.info("STEP :  " + stepNumber
		    + " : EXPECTED: THE VALUE SHOULD BE SET FOR THE CORRESPONDING WEBPA PARAMETER");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "FAILED TO SET THE WEBPA PARAMETER AFTER FIRMWARE UPGRADE.";
	    status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_ACCESSPOINT_2_4_GHZ_PRIVATE_SECURITY_MODEENABLED,
		    WebPaDataTypes.STRING.getValue(), BroadBandTestConstants.SECURITY_MODE_WPA2_PERSONAL);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL : SUCCESSFULLY VERIFIED WEBPA REQUEST 'SET' IS COMMUNICATING WITH THE DEVICE AFTER FIRMWARE UPGRADE");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 7 : VERIFY TRIGGERED CDL SUCCESSFULL WITH PREVIOUS FIRMWARE VERSION
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    status = false;
	    LOGGER.info("***************************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : VERIFY TRIGGERED CDL SUCCESSFULL WITH PREVIOUS FIRMWARE VERSION ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : TRIGGER THE CDL WITH PREVIOUS FIRMWARE VERSION USING TR-181/SNMP");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : CDL MUST SUCCESSFUL WITH PREVIOUS FIRMWARE VERSION.");
	    LOGGER.info("***************************************************************************************");
	    errorMessage = "UNABLE TO TRIGGER THE IMAGE UPGRADE ON THE DEVICE WITH PREVIOUS FIRMWARE VERSION.";
	    hasOriginalBuildChanged = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
		    initialFirmwareVersion);
	    status = hasOriginalBuildChanged;
	    LOGGER.info("FLASHED THE ORIGINAL BUILD ON THE DEVICE: " + status);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : CDL SUCCESSFUL WITH PREVIOUS FIRMWARE VERSION.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("***************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFING WEBPA NOTIFICATIONS ON FIRMWARE UPGRADE : " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");

	    result = false;
	    status = false;
	    try {
		errorMessage = "Failed to SSH to Device";
		status = CommonMethods.isSTBAccessible(device)
			? CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)
			: CommonMethods.waitForEstbIpAcquisition(tapEnv, device);
		if (status && !hasOriginalBuildChanged) {
		    LOGGER.info("Reverting back to the default image:" + currentImageNameforPostCondition);
		    FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);

		    result = BroadBandCodeDownloadUtils.triggerPreviousCodeDownload(device, tapEnv,
			    currentImageNameforPostCondition);
		    errorMessage = "Failed to downgrade image to default image";
		    FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
		}

		if (result) {
		    LOGGER.info("POST-CONDITION : ACTUAL: Image Reverted Successfully");
		} else {
		    LOGGER.info("POST-CONDITION : ACTUAL: Device has initial build. No downgrade required");
		}
		LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	    } catch (Exception exception) {
		LOGGER.error("ERROR: " + errorMessage);
	    }
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-5003");
    }

    /**
     * Verify whether user is able to run user or manual script even after giving 777 permission
     * <ol>
     * <li>Create temporary executable file in nvram</li>
     * <li>Execute temporary executable file in nvram</li>
     * <li>Change permissions of the temporary executable file in nvram</li>
     * <li>validate permissions of file in nvram</li>
     * <li>Execute temporary executable file in nvram</li>
     * <li>Create temporary executable file in nvram2</li>
     * <li>Execute temporary executable file in nvram2</li>
     * <li>Change permissions of the temporary executable file in nvram2</li>
     * <li>validate permissions of file in nvram2</li>
     * <li>Execute temporary executable file in nvram2</li>
     * </ol>
     * 
     * @param device
     *            Dut Instance
     * @author prasanthreddy
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-EXEC-FILE-CHK-1001")
    public void testToValidateExecutnWithPermsn(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-EXEC-FILE-CHK-101";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String response = "";
	boolean isProd = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-EXEC-FILE-CHK-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify whether user is able to run user or manual script even after giving 777 permission");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Create temporary executable file in nvram");
	LOGGER.info("2. Execute temporary executable file in nvram");
	LOGGER.info("3. Change permissions of the temporary executable file  in nvram");
	LOGGER.info("4. validate permissions of file in nvram");
	LOGGER.info("5. Execute temporary executable file in nvram");
	LOGGER.info("6. Create temporary executable file in nvram2");
	LOGGER.info("7. Execute temporary executable file  in nvram2");
	LOGGER.info("8. Change permissions of the temporary executable file   in nvram2");
	LOGGER.info("9. validate permissions of file  in nvram2");
	LOGGER.info("10. Execute temporary executable file  in nvram2");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");

	    isProd = CommonMethods.isProdImage(tapEnv, device);
	    LOGGER.info("Is Device in prod build : " + isProd);

	    stepNum = "S1";
	    errorMessage = "Unable to create file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Create temporary executable file in nvram");
	    LOGGER.info("STEP 1: ACTION : Execute: touch /nvram/test.sh");
	    LOGGER.info("STEP 1: EXPECTED : Successfully created temporary executable file");
	    LOGGER.info("**********************************************************************************");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.NVRAM_PATH,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));

		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.CMD_TAG_FOR_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.NVRAM_PATH,
				BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES, BroadBandCommandConstants.CMD_GREP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));

		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Successfully created temporary executable file in nvram");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S2";
	    errorMessage = "able to execute file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Execute temporary executable file in nvram");
	    LOGGER.info("STEP 2: ACTION : Execute : sh /nvram/test.sh");
	    LOGGER.info("STEP 2: EXPECTED : Successfully verified unable to execute to file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.NVRAM_PATH, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfully verified unable to execute file");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Unable to change permissions of the file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Change permissions of the temporary executable file  in nvram");
	    LOGGER.info("STEP 3: ACTION : Execute :chmod 777/nvram/ test.sh ");
	    LOGGER.info("STEP 3: EXPECTED : Successfully changed permissions of the file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_CHMOD, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.CHMOD_777_VALUE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandTestConstants.NVRAM_PATH, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = CommonMethods.isNull(response) && !CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully changed permissions of the file");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S4";
	    errorMessage = "Unable to validate permissions of the file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : validate permissions of file in nvram");
	    LOGGER.info("STEP 4: ACTION : Execute : ls -la /nvram | grep test.sh");
	    LOGGER.info("STEP 4: EXPECTED : Successfully verified permissions of the file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.CMD_TAG_FOR_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandTestConstants.NVRAM_PATH,
				BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES, BroadBandCommandConstants.CMD_GREP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.FULL_FILE_PERMISSIONS_FOR_FILE);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Successfully verified permission of the file");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S5";
	    errorMessage = isProd ? "able to execute file" : "Unable to execute file ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Execute temporary executable file in nvram");
	    LOGGER.info("STEP 5: ACTION : Execute : sh /nvram/test.sh");
	    LOGGER.info("STEP 5: EXPECTED :" + (isProd ? " Successfully verified not able to execute file"
		    : " Successfully verified able to execute file"));
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.NVRAM_PATH, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = isProd
			? CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.ERROR_PERMISSION_DENIED)
			: CommonMethods.isNull(response) && !CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : " + (isProd ? " Successfully verified not able to execute file"
			: " Successfully verified able to execute file"));
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S6";
	    errorMessage = "Unable to create file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Create temporary executable file in nvram2");
	    LOGGER.info("STEP 6: ACTION : Execute: touch /nvram2/test.sh");
	    LOGGER.info("STEP 6: EXPECTED : Successfully created temporary executable file");
	    LOGGER.info("**********************************************************************************");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_TOUCH,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.MOUNT_NVRAM2,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));

		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.CMD_TAG_FOR_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.MOUNT_NVRAM2,
				BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES, BroadBandCommandConstants.CMD_GREP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));

		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully created temporary executable file in nvram");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S7";
	    errorMessage = "able to execute file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Execute temporary executable file  in nvram2");
	    LOGGER.info("STEP 7: ACTION : Execute : sh /nvram2/test.sh");
	    LOGGER.info("STEP 7: EXPECTED : Successfully verified unable to execute to file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.MOUNT_NVRAM2, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Successfully verified unable to execute file");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S8";
	    errorMessage = "Unable to change permissions of the file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Change permissions of the temporary executable file in nvram2");
	    LOGGER.info("STEP 8: ACTION : Execute :chmod 777 /nvram2/test.sh ");
	    LOGGER.info("STEP 8: EXPECTED : Successfully changed permissions of the file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_CHMOD, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.CHMOD_777_VALUE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			BroadBandCommandConstants.MOUNT_NVRAM2, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = CommonMethods.isNull(response) && !CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Successfully changed permissions of the file");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to validate permissions of the file";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : validate permissions of file  in nvram2");
	    LOGGER.info("STEP 9: ACTION : Execute : ls -la /nvram2 | grep test.sh");
	    LOGGER.info("STEP 9: EXPECTED : Successfully verified permissions of the file");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.CMD_TAG_FOR_LS,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.MOUNT_NVRAM2,
				BroadBandTestConstants.SYMBOL_PIPE_WITH_SPACES, BroadBandCommandConstants.CMD_GREP,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));

		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.FULL_FILE_PERMISSIONS_FOR_FILE);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Successfully verified permission of the file");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S10";
	    errorMessage = "Unable to execute file ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Execute temporary executable file  in nvram2");
	    LOGGER.info("STEP 10: ACTION : Execute : sh /nvram2/test.sh");
	    LOGGER.info("STEP 10: EXPECTED : " + (isProd ? " Successfully verified not able to execute file"
		    : " Successfully verified able to execute file"));
	    LOGGER.info("**********************************************************************************");
	    try {
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.MOUNT_NVRAM2, BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
		status = isProd
			? CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.ERROR_PERMISSION_DENIED)
			: CommonMethods.isNull(response) && !CommonUtils.patternSearchFromTargetString(response,
				BroadBandTestConstants.ERROR_PERMISSION_DENIED);
	    } catch (Exception e) {
		LOGGER.error("Error in executing command in device " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL :" + (isProd ? " Successfully verified not able to execute file"
			: " Successfully verified able to execute file"));
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Delete temporary files in nvram and nvram2");
	    LOGGER.info("POST-CONDITION : ACTION : Execute : rm /nvram/test.sh ; rm /nvram2/test.sh");
	    LOGGER.info("POST-CONDITION : EXPECTED : Successfully deleted files");
	    LOGGER.info("######################################################################");
	    status = CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.NVRAM_PATH,
			    BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE))
		    && CommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.MOUNT_NVRAM2,
				    BroadBandTestConstants.TEMPORARY_EXECUTABLE_FILE));
	    if (status) {
		LOGGER.info("POST-CONDITION : ACTUAL : Successfully deleted files");
	    } else {
		LOGGER.error("POST-CONDITION : ACTUAL : Unable to delete files");
	    }
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-EXEC-FILE-CHK-1001");
    }

    /**
     * Test to verify sshbanners from sshbanner.txt file and command execution
     * 
     * STEP 1:Verify sshbanner.txt file availability from /etc/ folder STEP 2:Verify sshbanner.txt file size and file
     * corrupted status STEP 3:Verify Legal/Compliance "ssh banner" from sshbannr.txt file STEP 4:Verify dropbear
     * processor running status STEP 5:Verify Legal/Compliance "ssh banner" after SSH to Device. STEP 6:Verify
     * Legal/Compliance "ssh banner" after SSH to Device in ATOM_CONSOLE.
     * 
     * @param device
     *            The Dut to be used.
     * 
     * @author Gnanaprakasham S
     * @refactor Govardhan
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, groups = {
	    BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1023")

    public void testToVerifySshBannersWithLeagalWarnings(Dut device) {

	// Variable to store Test step number
	String testStepNumber = "s1";
	// Variable to store the test case ID
	String testId = "TC-RDKB-SYSTEM-023";
	// Variable to store execution status
	boolean status = false;
	// Variable to store error message
	String errorMessage = null;
	// Variable to store command response
	String response = null;
	// Hash map to store file details and size
	try {

	    LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-023");

	    LOGGER.info("**************************************************************");
	    LOGGER.info("TEST DESCRIPTION: Verify ssh banner presence with legal warning message");
	    LOGGER.info("*************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: Verify sshbanner.txt file availability from /etc/ folder");
	    LOGGER.info("STEP 2: Verify sshbanner.txt file size and file corrupted status ");
	    LOGGER.info("STEP 3:  Verify whether sshbanner.txt file has Legal/Compliance \"ssh banner\" ");
	    LOGGER.info("STEP 4: Verify dropbear processor running status ");
	    LOGGER.info("STEP 5: Verify Legal/Compliance \"ssh banner\" after SSH to Device in Arm and Atom console. ");

	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify sshbanner.txt file availability from /etc/ folder");
	    LOGGER.info("STEP 1: ACTION : Execute command:ls /etc | grep \"ssh\"");
	    LOGGER.info("STEP 1: EXPECTED : sshbanner.txt file should be present in /etc folder");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandAndReturnResponseWithBanner(device,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
			    BroadBandTestConstants.STRING_PATH_SSH_BANNER_TXT_FILE));

	    if (CommonMethods.isNotNull(response)) {
		LOGGER.info("Successfully obtained file list from /etc folder : " + response);
		status = true;
		errorMessage = status ? null
			: BroadBandTestConstants.SSH_BANNER_TXT_FILE_NAME + " file is not available in /etc folder ";
	    } else {
		errorMessage = BroadBandTestConstants.STRING_PATH_SSH_BANNER_TXT_FILE + " is not available";
		LOGGER.info(errorMessage);
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : sshbanner.txt file is present in /etc folder");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s2";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify sshbanner.txt file size and file corrupted status");
	    LOGGER.info("STEP 2: ACTION : Execute command:ls /etc | grep \"ssh\"");
	    LOGGER.info(
		    "STEP 2: EXPECTED : sshbanner.txt file size should be greater than zero and it should not be in corrupted state");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(response)) {
		LOGGER.info("Successfully Obtained file size for sshbanner.txt file :  " + response);
		int size = response.length();
		status = size > RDKBTestConstants.CONSTANT_0;
		errorMessage = status ? null
			: BroadBandTestConstants.SSH_BANNER_TXT_FILE_NAME
				+ " file size is in 0 KB. it is not a valid file!!! ";
	    } else {
		errorMessage = "Failed to get the response for sshbanner.txt file size!!! ";
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : sshbanner.txt is greater than zero and its not in corrupted state");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Legal/Compliance \"ssh banner\" from sshbannr.txt file");
	    LOGGER.info("STEP 3: ACTION : Execute command:cat /etx/sshbanner.txt");
	    LOGGER.info("STEP 3: EXPECTED : Legal/Compliance \"ssh banner\" must be shown with legal warnings");
	    LOGGER.info("**********************************************************************************");

	    String sshBanner = BroadbandPropertyFileHandler.getSSHBanner();
	    // removing all white spaces from ssh banner for easy comparison
	    sshBanner = sshBanner.replaceAll(RDKBTestConstants.SINGLE_SPACE_CHARACTER, RDKBTestConstants.EMPTY_STRING);

	    response = tapEnv.executeCommandAndReturnResponseWithBanner(device,
		    BroadBandTestConstants.ECHO_WITH_SPACE + RDKBTestConstants.STRING_CONNECTION_STATUS);
	    LOGGER.info("Echo Response is : " + response);
	    if (CommonMethods.isNotNull(response)) {
		LOGGER.info(" Obtained ssh banners from sshbanner.txt file from VM execution : " + response);
		response = response.replaceAll(RDKBTestConstants.PATTERN_GET_NEWLINE_SPACE,
			RDKBTestConstants.EMPTY_STRING);

		if (CommonMethods.isNotNull(response)) {
		    LOGGER.info(" Successfully Obtained ssh banners after removing white space : " + response);
		    status = response.contains(sshBanner);
		    errorMessage = status ? null : "Current image don't have proper ssh banners.";
		}

	    } else {
		errorMessage = "Obtained null response..Failed to get the ssh banners from sshbanner.txt file !!!!";
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Legal/Compliance \"ssh banner\" is shown with legal warnings");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify dropbear processor running status");
	    LOGGER.info("STEP 4: ACTION : Execute command:ps | grep dropbear");
	    LOGGER.info("STEP 4: EXPECTED : Dropbear processor should be up and running without any issue");
	    LOGGER.info("**********************************************************************************");

	    response = CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.STRING_DROPBEAR_PROCESSOR);
	    LOGGER.info("PID of Process: " + response);
	    if (CommonMethods.isNotNull(response)) {
		status = true;
	    } else {
		errorMessage = "Failed to get the process ID for drop bear process !!!!!!";
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Dropbear processor is up and running without any issue");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s5";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify Legal/Compliance \"ssh banner\" after SSH to Device.");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the command :sudo stbsshv6 <IPv6 Address> Erouter IPv6 address for Fiber Devices");
	    LOGGER.info("STEP 5: EXPECTED : Legal/Compliance \"ssh banner\" must be shown with legal warnings");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandAndReturnResponseWithBanner(device,
		    BroadBandTestConstants.ECHO_WITH_SPACE + RDKBTestConstants.STRING_CONNECTION_STATUS);
	    try {
		BroadBandCommonUtils.verifySshBannerPresenceWithLegalWarnings(response);
		status = true;
		LOGGER.info("Successfilly verified proper ssh banners are there in arm console");
	    } catch (TestException e) {
		errorMessage = e.getMessage();
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Legal/Compliance \"ssh banner\" is shown with legal warnings");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    testStepNumber = "s6";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify Legal/Compliance \"ssh banner\" after SSH to Device in Atom Console.");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the command : sudo sshtoatom <IPv6 Address> <Internal ATOM IP> <IPv6 Address> : ECM IPv6 Address for RDKB devices");
	    LOGGER.info("STEP 6: EXPECTED : Legal/Compliance \"ssh banner\" must be shown with legal warnings");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {

		response = tapEnv.executeCommandAndReturnResponseWithBanner(device,
			BroadBandTestConstants.ECHO_WITH_SPACE + RDKBTestConstants.STRING_CONNECTION_STATUS);
		try {
		    BroadBandCommonUtils.verifySshBannerPresenceWithLegalWarnings(response);
		    LOGGER.info("Successfilly verified proper ssh banners are there in atom console");
		    status = true;
		} catch (TestException e) {
		    errorMessage = e.getMessage();
		}

		if (status) {
		    LOGGER.info("STEP 6: Legal/Compliance \"ssh banner\" must be shown with legal warnings");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}

		tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	    } else {
		LOGGER.info("STEP 6: This Step is applicable for Atom Sync devices only");
		tapEnv.updateExecutionForAllStatus(device, testId, testStepNumber, ExecutionStatus.NOT_APPLICABLE,
			"This Step is applicable for Atom Sync devices only", false);
	    }
	    LOGGER.info("Ending TEST CASE: TC-RDKB-SYSTEM-023");
	} catch (Exception exception) {
	    LOGGER.error("Exception occurred during execution : " + exception.getMessage());
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);
	}
    }

    /**
     * Validate if RDKB SSH client is not accessible via WAN.
     * <ol>
     * <li>Verify WEBPA command to get the WANIPV6 of the Client is Successful.</li>
     * <li>Validate if we are able to SSH to the WAN IP using the Jump server</li>
     * </ol>
     * 
     * @param settop
     *            {@link Settop}
     * @author Joseph M
     * @refactor Alan_Bivera
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-9008")

    public void testToVerifySshForWanIp(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SYSTEM-908";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	String wanIpv6Address = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-9008");
	LOGGER.info("TEST DESCRIPTION: Validate if Comcast RDKB SSH client is not accessible via WAN.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WEBPA command to get the WANIPV6 of the Client is Successful.");
	LOGGER.info("2. Validate if we are  able to SSH to the WAN IP using the Jump server ");

	LOGGER.info("#######################################################################################");

	try {
	    stepNum = "S1";
	    errorMessage = "Unable to get the WAN IPV6 Address Using the WEBPA Parameter.-" + wanIpv6Address;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify WEBPA command to get the WANIPV6 of the Client is Successful. ");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the Below Command:curl -X GET -H \"Authorization: Bearer <SAT TOKEN>\" -H \"content-type:application/json\" -H \"X-Webpa-Atomic:true\" -k -i <url:port>/api/v2/device/mac:<MAC Address>/config?names=Device.DeviceInfo.X_COMCAST-COM_WAN_IPv6");
	    LOGGER.info(
		    "STEP 1: EXPECTED : WebPA get should be successful and WANIPV6 should be Retrieved succesfully.");
	    LOGGER.info("**********************************************************************************");
	    wanIpv6Address = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WAN_IPV6);
	    LOGGER.info("Wan Ipv6 Address is = " + wanIpv6Address);
	    status = CommonMethods.isNotNull(wanIpv6Address) && CommonMethods.isIpv6Address(wanIpv6Address);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WanIpv6 Address should be retrieved successfully");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    stepNum = "S2";
	    errorMessage = "SSH client is accessible via WAN IP. ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Validate if we are  able to SSH to the WAN IP using the Jump server ");
	    LOGGER.info("STEP 2: ACTION : SSH to the wanip of the device using the jump server");
	    LOGGER.info("STEP 2: EXPECTED : SSH client must not accessible via WAN IP. ");
	    LOGGER.info("**********************************************************************************");
	    status = !BroadBandCommonUtils.executeSshCommandOnJumpServer(tapEnv, device, wanIpv6Address);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : SSH client is not accessible via WAN IP");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-9008");
    }

    /**
     * Verify accessibilty for ARM to Atom SSH.
     * <ol>
     * <li>Verify Atom console is accessible from ARM console</li>
     * <li>Verify CR process running status in atom console from ARM console</li>
     * </ol>
     * 
     * @author Gnanaprakasham
     * @refactor Athira
     * 
     * @param device
     *            {@link Dut}
     * 
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SYSTEM-1029")

    public void verifyaccessibilityFromArmConsoleToAtomConsole(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SYSTEM-129";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SYSTEM-1029");
	LOGGER.info("TEST DESCRIPTION: Verify accessibilty for ARM to Atom SSH");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify Atom console is accessible from ARM console");
	LOGGER.info("2. Verify CR process running status in atom console from ARM console");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "Not able to access atom console from ARM console";
	    status = false;
	    String ipAddress = null;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify Atom console is accessible from ARM console");
	    LOGGER.info("STEP 1: ACTION : Execute the below command to verify access status");
	    LOGGER.info("STEP 1: EXPECTED : Atom console should be accessible from ARM console.");
	    LOGGER.info("**********************************************************************************");

	    // Atom console IP obtained from property file for specific devices
	    try {

		ipAddress = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandTestConstants.ATOM_CONSOLE_IP_SPECIFIC_DEVICES);
	    } catch (Exception e) {

		LOGGER.info("No device specific value found, test case is not appicable for the Platform");
	    }
	    if (CommonMethods.isNotNull(ipAddress)) {
		String[] commands = {
			AutomaticsTapApi
				.getSTBPropsValue(BroadBandPropertyKeyConstants.CMD_TOACCESS_ATOMCONSOLEFROMARM),
			AutomaticsTapApi.getSTBPropsValue(BroadBandPropertyKeyConstants.CMD_TOACCESS_ATOMCONSOLEFROMARM)
				.replace("<ip address>", ipAddress) + BroadBandTestConstants.SINGLE_SPACE_CHARACTER
				+ BroadBandTestConstants.CMD_ECHO + BroadBandTestConstants.SINGLE_SPACE_CHARACTER + "\""
				+ BroadBandTestConstants.STRING_VERIFY_ATOM_ACCESSIBILITY + "\"" };

		BroadBandResultObject broadBandResultObject = verifyAccessLogsFromArmConsole(device, tapEnv, commands,
			BroadBandTestConstants.STRING_VERIFY_ATOM_ACCESSIBILITY, "atom");

		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();

		if (status) {
		    LOGGER.info("STEP 1: ACTUAL : SUCCESSFULLY VERIFIED ATOM CONSOLE IS ACCESSSIBLE FROM ARM CONSOLE");
		} else {
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		LOGGER.info("**********************************************************************************");

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

		stepNum = "S2";
		errorMessage = "Failed to verify CR process running status via ARM to atom connection";
		status = false;

		LOGGER.info("**********************************************************************************");
		LOGGER.info("STEP 2: DESCRIPTION : Verify CR process running status in atom console from ARM console");
		LOGGER.info("STEP 2: ACTION : Execute the command atond verify access status ");
		LOGGER.info(
			"STEP 2: EXPECTED : CR process must be running and response should contains CR process pid id");
		LOGGER.info("**********************************************************************************");

		commands[1] = commands[1].replace(
			BroadBandTestConstants.CMD_ECHO + BroadBandTestConstants.SINGLE_SPACE_CHARACTER + "\""
				+ BroadBandTestConstants.STRING_VERIFY_ATOM_ACCESSIBILITY + "\"",
			BroadBandTestConstants.PS_COMMAND_FOR_CCSP_PROCESS);

		broadBandResultObject = verifyAccessLogsFromArmConsole(device, tapEnv, commands,
			BroadBandTestConstants.CCSP_COMPONENT_LIST_ATOM.get(1),
			BroadBandTestConstants.STRING_ATOM_CONSOLE);

		status = broadBandResultObject.isStatus();
		errorMessage = broadBandResultObject.getErrorMessage();

		if (status) {
		    LOGGER.info(
			    "STEP 2: ACTUAL : SUCCESSFULLY VERIFIED CR PROCESS RUNNING STATUS IN ATOM CONSOLE FROM ARM CONSOLE");
		} else {
		    LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
		}

		LOGGER.info("**********************************************************************************");

		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, broadBandResultObject.isStatus(),
			broadBandResultObject.getErrorMessage(), true);
	    } else {
		status = false;
		errorMessage = "The test case is not appicable for the Platform";
		tapEnv.updateExecutionStatus(device, testCaseId, stepNum, false, errorMessage, true);
	    }

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE,
		    errorMessage, false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SYSTEM-1029");
    }

    /**
     * Verify the swap memory can be enabled and disabled using ZRAM
     * <ol>
     * <li>Configure and post RFC payload data to enable swap memory when RFC config service is triggered</li>
     * <li>Restart RFC config service to update settings</li>
     * <li>Verify Memory Swap has been enabled by RFC script</li>
     * <li>Verify the gateway can be rebooted using WebPA</li>
     * <li>Verify the memory swap is still enabled after reboot</li>
     * <li>Verify the memory swap enable using ZRAM is logged in Console log file</li>
     * <li>Verify ZRAM partitions on the device after enabling swap memory</li>
     * <li>Configure and post RFC payload data to disable swap memory when RFC config service is triggered</li>
     * <li>Restart RFC config service to update settings</li>
     * <li>Verify Memory Swap has been disabled by RFC script</li>
     * <li>Verify the gateway can be rebooted using WebPA to disable memory swap</li>
     * <li>Verify the memory swap is still disabled after reboot</li>
     * <li>Verify the memory swap disable using ZRAM is logged in Console log file</li>
     * </ol>
     * 
     * @refactor Govardhan
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-SWAP-5001")
    public void testToEnableSwap(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-SWAP-501";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SWAP-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the swap memory can be enabled and disabled using ZRAM");
	LOGGER.info("TEST STEPS : ");

	LOGGER.info(
		"1. Configure and post RFC payload data to enable swap memory when RFC config service is triggered");
	LOGGER.info("2. Restart RFC config service to update settings");
	LOGGER.info("3. Verify Memory Swap has been enabled by RFC script");
	LOGGER.info("4. Verify the gateway can be rebooted using WebPA to enable swap during bootup");
	LOGGER.info("5. Verify the memory swap is still enabled after reboot");
	LOGGER.info("6. Verify the memory swap enable using ZRAM is logged in Console log file");
	LOGGER.info("7. Verify ZRAM partitions on the device after enabling swap memory");
	LOGGER.info(
		"8. Configure and post RFC payload data to disable swap memory when RFC config service is triggered");
	LOGGER.info("9. Restart RFC config service to update settings");
	LOGGER.info("10. Verify Memory Swap has been disabled by RFC script");
	LOGGER.info("11. Verify the gateway can be rebooted using WebPA to disable memory swap");
	LOGGER.info("12. Verify the memory swap is still disabled after reboot");
	LOGGER.info("13. Verify the memory swap disable using ZRAM is logged in Console log file");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "S1";
	    errorMessage = "Unable to post the RFC payload data to Proxy Xconf DCM Server";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Configure and post RFC payload data to enable swap memory when RFC config service is triggered");
	    LOGGER.info("STEP 1: ACTION : Post the RFC payload data to Proxy Xconf DCM Server");
	    LOGGER.info("STEP 1: EXPECTED : RFC payload data should be successfully posted to Proxy Xconf DCM Server");
	    LOGGER.info("**********************************************************************************");
	    JSONObject jsonObject = new JSONObject();
	    try {
		jsonObject.put(
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP),
			BroadBandTestConstants.TRUE);
	    } catch (JSONException e) {
		errorMessage = "Exception occurred while configuring RFC payload data : " + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    BroadBandTestConstants.STRING_RFC_DATA_PAYLOAD
			    .replace(BroadBandTestConstants.STRING_REPLACE, jsonObject.toString())
			    .replace(BroadBandTestConstants.CONSTANT_REPLACE_STBMAC_LSAPAYLOADDATA,
				    device.getHostMacAddress()));
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : RFC payload data to enable Memory Swap is successfully posted to Proxy Xconf DCM Server");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "Unable to restart RFC Service";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Restart RFC config service to update settings");
	    LOGGER.info("STEP 2: ACTION : Execute the command : sh /lib/rdk/RFCbase.sh");
	    LOGGER.info("STEP 2: EXPECTED : RFC config should restart successfully");
	    LOGGER.info("**********************************************************************************");
	    String response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.RFC_RESTART_SERVICE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.RFC_RESTART_SUCCEEDED);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : RFC service restart has been successfully trigerred");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S3";
	    errorMessage = "Unable to validate Memory Swap enable status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify Memory Swap has been enabled by RFC script");
	    LOGGER.info(
		    "STEP 3: ACTION : Validate the TR-181 'Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.MEMSWAP.Enable' param using WebPA");
	    LOGGER.info("STEP 3: EXPECTED : Memory Swap should be enabled successfully by RFC");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP);
	    status = CommonMethods.isNotNull(response) && BroadBandTestConstants.TRUE.equalsIgnoreCase(response);
	    if (!status) {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.TRUE);
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Validated using WebPA, Memory Swap is enabled successfully using RFC");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s4";
	    errorMessage = "Unable to reboot the gateway!";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the gateway can be rebooted using WebPA  to enable swap during bootup");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the command : curl -4 -k -H \"Authorization: Bearer <SAT_TOKEN>\"  -X PATCH <WEBPA_URL><CM_MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.RebootDevice\",\"value\":\"Device\"}]}\"");
	    LOGGER.info("STEP 4: EXPECTED : Device should come online");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);
		LOGGER.info("STEP 4: ACTUAL : Device reboot is successful and device is up");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s5";
	    errorMessage = "Unable to check memory swap status using WebPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify the memory swap is still enabled after reboot");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute webPA get command on TR-181 param \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.MEMSWAP.Enable\" ");
	    LOGGER.info("STEP 5: EXPECTED : The returned value should be \"true\"");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP);
	    errorMessage = "Null/invalid response retrieved. Expected : true; Response : " + response;
	    status = CommonMethods.isNotNull(response) && BroadBandTestConstants.TRUE.equalsIgnoreCase(response);
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Memory swap is successfully retaining its status after reboot");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s6";
	    errorMessage = "Memory swap enable operation using ZRAM is not logged in Console log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify the memory swap enable using ZRAM is logged in Console log file");
	    LOGGER.info(
		    "STEP 6: ACTION : Check console logs for logging on ZRAM enable using \"cat /rdklogs/logs/zram.log | grep zram\" command");
	    LOGGER.info("STEP 6: EXPECTED : ZRAM enable should be logged under cat /rdklogs/logs/zram.log");
	    LOGGER.info("**********************************************************************************");
	    response = CommonMethods.isAtomSyncAvailable(device, tapEnv)
		    ? tapEnv.executeCommandOnAtom(device,
			    BroadBandCommandConstants.CMD_TO_GREP_ENABLE_AND_DISABLE_ZRAM_LOG)
		    : tapEnv.executeCommandUsingSsh(device,
			    BroadBandCommandConstants.CMD_TO_GREP_ENABLE_AND_DISABLE_ZRAM_LOG);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTraceConstants.LOG_MESSAGE_ENABLING_ZRAM);
	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Memory swap enable operation using ZRAM is successfully logged in zram.log file");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s7";
	    errorMessage = "Unable to check ZRAM partition in the device";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify ZRAM partitions in the device after enabling swap memory");
	    LOGGER.info("STEP 7: ACTION : Check for the ZRAM partitions in the device using \"cat /proc/swaps\"");
	    LOGGER.info("STEP 7: EXPECTED : ZRAM partitions should be present in the device");
	    LOGGER.info("**********************************************************************************");
	    response = CommonMethods.isAtomSyncAvailable(device, tapEnv)
		    ? tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_TO_GET_ZRAM_PARTITION)
		    : tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_TO_GET_ZRAM_PARTITION);
	    if (CommonMethods.isNotNull(response)) {
		List<String> partitions = CommonMethods.patternFinderToReturnAllMatchedString(response,
			BroadBandTestConstants.PATTERN_TO_GET_ZRAM_PARTITION_FILE_NAMES);
		errorMessage = "The gateway don't have any ZRAM partition even after enabling memory swap";
		if (partitions.size() != 0 && !partitions.isEmpty()) {
		    errorMessage = "Expected ZRAM partitions in AtomSync is 1 and other platforms should have 2. But Actual no of partitions is "
			    + partitions.size();
		    status = partitions.size() == BroadBandTestConstants.CONSTANT_2;
		}
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : ZRAM partitions is successfully present in the device");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S8";
	    errorMessage = "Unable to post the RFC payload data to Proxy Xconf DCM Server";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Configure and post RFC payload data to disable swap memory when RFC config service is triggered");
	    LOGGER.info("STEP 8: ACTION : Post the RFC payload data to Proxy Xconf DCM Server");
	    LOGGER.info("STEP 8: EXPECTED : RFC payload data should be successfully posted to Proxy Xconf DCM Server");
	    LOGGER.info("**********************************************************************************");
	    jsonObject = new JSONObject();
	    try {
		jsonObject.put(
			CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.TR181_DOT,
				BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP),
			BroadBandTestConstants.FALSE);
	    } catch (JSONException e) {
		errorMessage = "Exception occurred while configuring RFC payload data : " + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    status = BroadBandRfcFeatureControlUtils.executePreconditionForRfcTests(device, tapEnv,
		    BroadBandTestConstants.STRING_RFC_DATA_PAYLOAD
			    .replace(BroadBandTestConstants.STRING_REPLACE, jsonObject.toString())
			    .replace(BroadBandTestConstants.CONSTANT_REPLACE_STBMAC_LSAPAYLOADDATA,
				    device.getHostMacAddress()));
	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : RFC payload data to disable Swap Memory is successfully posted to Proxy Xconf DCM Server");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S9";
	    errorMessage = "Unable to restart RFC Service";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Restart RFC config service to update settings");
	    LOGGER.info("STEP 9: ACTION : Execute the command : sh /lib/rdk/RFCbase.sh");
	    LOGGER.info("STEP 9: EXPECTED : RFC config should restart successfully");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.RFC_RESTART_SERVICE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.RFC_RESTART_SUCCEEDED);
	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : RFC service restart has been successfully trigerred");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "S10";
	    errorMessage = "Unable to validate Memory Swap disable status";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify Memory Swap has been disabled by RFC script");
	    LOGGER.info(
		    "STEP 10: ACTION : Validate the TR-181 'Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.MEMSWAP.Enable' param using WebPA");
	    LOGGER.info("STEP 10: EXPECTED : Memory Swap should be disabled successfully by RFC");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP);
	    status = CommonMethods.isNotNull(response) && BroadBandTestConstants.FALSE.equalsIgnoreCase(response);
	    if (!status) {
		status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP, BroadBandTestConstants.CONSTANT_3,
			BroadBandTestConstants.FALSE);
	    }
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Validated using WebPA, Memory Swap is disabled successfully using RFC");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s11";
	    errorMessage = "Unable to reboot the gateway!";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify the gateway can be rebooted using WebPA to disable memory swap");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the command : curl -4 -k -H \"Authorization: Bearer <SAT_TOKEN>\"  -X PATCH <WEBPA_URL><CM_MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"Device.X_CISCO_COM_DeviceControl.RebootDevice\",\"value\":\"Device\"}]}\"");
	    LOGGER.info("STEP 11: EXPECTED : Device should come online");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTES);
		LOGGER.info("STEP 11: ACTUAL : Device reboot is successful and device is up");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s12";
	    errorMessage = "Unable to check memory swap status using WebPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify the memory swap is still disabled after reboot");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute webPA get command on TR-181 param \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.MEMSWAP.Enable\" ");
	    LOGGER.info("STEP 12: EXPECTED : The returned value should be \"false\"");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_MEMORY_SWAP);
	    errorMessage = "Null/invalid response retrieved. Expected : false; Response : " + response;
	    status = CommonMethods.isNotNull(response) && BroadBandTestConstants.FALSE.equalsIgnoreCase(response);
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Memory swap is successfully retaining its status after reboot");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "s13";
	    errorMessage = "Memory swap disable operation using ZRAM is not logged in Console log file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 13: DESCRIPTION : Verify the memory swap disable using ZRAM is logged in Console log file");
	    LOGGER.info("STEP 13: ACTION : Check console logs for logging on ZRAM enable");
	    LOGGER.info("STEP 13: EXPECTED : ZRAM disable should be logged under cat /rdklogs/logs/zram.log");
	    LOGGER.info("**********************************************************************************");
	    response = CommonMethods.isAtomSyncAvailable(device, tapEnv)
		    ? tapEnv.executeCommandOnAtom(device,
			    BroadBandCommandConstants.CMD_TO_GREP_ENABLE_AND_DISABLE_ZRAM_LOG)
		    : tapEnv.executeCommandUsingSsh(device,
			    BroadBandCommandConstants.CMD_TO_GREP_ENABLE_AND_DISABLE_ZRAM_LOG);
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTraceConstants.LOG_MESSAGE_DISABLING_ZRAM);
	    if (status) {
		LOGGER.info(
			"STEP 13: ACTUAL : Memory swap disable operation using ZRAM is successfully logged in zram.log file");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-SWAP-5001");
    }

}
