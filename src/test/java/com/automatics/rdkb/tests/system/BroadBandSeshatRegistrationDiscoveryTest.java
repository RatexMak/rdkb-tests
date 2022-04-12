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

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.ResultValues;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Class for Validating Seshat (service registration and discovering component) distro
 */
public class BroadBandSeshatRegistrationDiscoveryTest extends AutomaticsTestBase {
    
    /**
     * Test case is to verify the seshat service registration and component discovery distro - disabled scenario
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Verify seshat is not running on atom console</li>
     * <li>Step 2: Verify the log messages present in Lmlite log file</li>
     * <li>Step 3: Verify the log messages present in Parodus log file</li>
     * <li>Step 4: Verify the log messages present in Harvester log file</li>
     * <li>Step 5: Verify seshat is not running on arm console</li>
     * <li>Step 6: Verify seshat binary is not present using systemctl command in atom console</li>
     * <li>Step 7: Verify seshat binary is not present using systemctl command in arm console</li>
     * </ol>
     * 
     * @author  Ashwin sankara
     * @Refactor Alan_Bivera
     * 
     * @param Dut
     *            {@link device}
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.SYSTEM })
    @TestDetails(testUID = "TC-RDKB-SESHAT-5001")

    public void verifySeshatRegistrationDiscoveryDisabled(Dut device) {

	String testCaseId = "TC-RDKB-SESHAT-501";
	boolean result = false;
	boolean isAtomSyncAvailable = false;
	String errorMessage = null;
	String step = "s1";
	String seshatPid = null;
	ResultValues commandOutput = null;
	List<String> messageList = new ArrayList<String>();
	long startTime = BroadBandTestConstants.CONSTANT_0;
	String systemCtlCommand = null;
	String systemCtlResponse = null;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-SESHAT-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the seshat service registration and component discovery distro - disabled scenario");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("Step 1: Verify seshat is not running on atom console");
	LOGGER.info("Step 2: Verify the log messages present in Lmlite log file");
	LOGGER.info("Step 3: Verify the log messages present in Parodus log file");
	LOGGER.info("Step 4: Verify the log messages present in Harvester log file");
	LOGGER.info("Step 5: Verify seshat is not running on arm console");
	LOGGER.info("Step 6: Verify seshat binary is not present using systemctl command in atom console");
	LOGGER.info("Step 7: Verify seshat binary is not present using systemctl command in arm console");
	LOGGER.info("#######################################################################################");
	try {
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    /*
	     * Step 1: Verify seshat is not running on atom console
	     */
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("Step 1: Verify seshat is not running on atom console");
	    LOGGER.info("Expected - Seshat is not running on atom console");
	    LOGGER.info("*****************************************************************************************");
	    isAtomSyncAvailable = CommonMethods.isAtomSyncAvailable(device, tapEnv);
	    if (isAtomSyncAvailable) {
		seshatPid = BroadBandCommonUtils.getPidOfProcessFromAtomConsole(device, tapEnv,
			BroadBandTestConstants.PROCESS_NAME_SESHAT);
		result = CommonMethods.isNull(seshatPid);
		if (result) {
		    LOGGER.info("STEP 1: ACTUAL : Seshat binary is not present in Atom console");
		} else {
		    LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "ATOM console is not available for this device model";
		LOGGER.info("STEP 1: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");
	    /*
	     * Step 2: Verify the log messages present in LMLite log file
	     */

	    step = "s2";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("Step 2: Verify the log messages present in Lmlite log file");
	    LOGGER.info("Expected - Output contains the following messages:");
	    LOGGER.info("LMLite:rdk initialzed");
	    LOGGER.info("LMLite: Init for parodus Success");
	    LOGGER.info("And should not contain:");
	    LOGGER.info("libseshat_: discover_service_data() status 200, type 6");
	    LOGGER.info("*****************************************************************************************");

	    startTime = System.currentTimeMillis();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_LMLITE_INITIALIZATION);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_LMLITE_INTEGRATION);
	    // Polling to check if LMLite Log is not available soon after reboot
	    do {
		commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			BroadBandCommandConstants.LOG_FILE_LMLITE, true);
		if (commandOutput.isResult()) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTES
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    result = commandOutput.isResult();
	    errorMessage = commandOutput.getMessage();
	    if (result) {
		messageList.clear();
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_LMLITE_DISCOVERY);
		commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			BroadBandCommandConstants.LOG_FILE_LMLITE, false);
		result = commandOutput.isResult();
		errorMessage = commandOutput.getMessage();
	    }
	    LOGGER.info("Actual - " + (result ? "LMLite log contains all expected messages" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /*
	     * Step 3: Verify the log messages present in Parodus log file
	     */
	    step = "s3";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("Step 3: Verify the log messages present in Parodus log file");
	    LOGGER.info("Expected - Output contains the following messages:");
	    LOGGER.info("PARODUS: Ping received with payload");
	    LOGGER.info("libseshat disabled, Hence proceeding without registration");
	    LOGGER.info("And should not contain:");
	    LOGGER.info("PARODUS: seshat_url is");
	    LOGGER.info("PARODUS: seshatlib initialized!");
	    LOGGER.info("PARODUS: seshatlib registered!");
	    LOGGER.info("PARODUS: seshatlib discovered");
	    LOGGER.info("*****************************************************************************************");

	    startTime = System.currentTimeMillis();
	    messageList.clear();
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_INTEGRATION_COMPLETE);
	    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_DISABLED);
	    // Polling to check if PARODUS Log is not available soon after reboot
	    do {
		commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			BroadBandCommandConstants.LOG_FILE_PARODUS, true);
		if (commandOutput.isResult()) {
		    break;
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTES
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    result = commandOutput.isResult();
	    errorMessage = commandOutput.getMessage();
	    if (result) {
		messageList.clear();
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_URL);
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_INITIALIZED);
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_REGISTERED);
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_SESHAT_URL_DISCOVERY);
		commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			BroadBandCommandConstants.LOG_FILE_PARODUS, false);
		result = commandOutput.isResult();
		errorMessage = commandOutput.getMessage();
	    }
	    LOGGER.info("Actual - " + (result ? "Parodus log contains all expected messages" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);

	    /*
	     * Step 4: Verify the log messages present in Harvester log file
	     */
	    step = "s4";
	    result = false;
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("Step 4: Verify the log messages present in Harvester log file");
	    LOGGER.info("Expected - Output contains the following messages:");
	    LOGGER.info("HARV : /tmp/harvester_initialized created");
	    LOGGER.info("Init for parodus Success");
	    LOGGER.info("And should not contain:");
	    LOGGER.info("libseshat_: discover_service_data() status 200, type 6");
	    LOGGER.info("*****************************************************************************************");
	    if (!DeviceModeHandler.isBusinessClassDevice(device)) {
		startTime = System.currentTimeMillis();
		messageList.clear();
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INITIALIZATION);
		messageList.add(BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_INTEGRATION);
		// Polling to check if Harvester Log is not available soon after reboot
		do {
		    commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			    BroadBandCommandConstants.FILE_HARVESTER_LOG, true);
		    if (commandOutput.isResult()) {
			break;
		    }
		} while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTES
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		result = commandOutput.isResult();
		errorMessage = commandOutput.getMessage();
		if (result) {
		    messageList.clear();
		    messageList.add(BroadBandTraceConstants.LOG_MESSAGE_HARVESTER_DISCOVERY);
		    commandOutput = BroadBandCommonUtils.checkFileForPatterns(device, tapEnv, messageList,
			    BroadBandCommandConstants.FILE_HARVESTER_LOG, false);
		    result = commandOutput.isResult();
		    errorMessage = commandOutput.getMessage();
		}
		LOGGER.info("Actual - " + (result ? "Harvester log contains all expected messages" : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "Harvester feature is not available in business devices";
		LOGGER.info("STEP 4: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }
	    
	    step = "s5";
	    errorMessage = "Seshat is running on arm console  ";
	    result = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify seshat is not running on arm console");
	    LOGGER.info("STEP 5: ACTION : Execute pidof seshat");
	    LOGGER.info("STEP 5: EXPECTED : Seshat process should not run on arm console");
	    LOGGER.info("**********************************************************************************");
	    if (isAtomSyncAvailable) {
		errorMessage = "Not applicable for this device model";
		LOGGER.info("STEP 5: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {
		seshatPid = CommonMethods.getPidOfProcess(device, tapEnv, BroadBandTestConstants.PROCESS_NAME_SESHAT);
		result = CommonMethods.isNull(seshatPid);

		if (result) {
		    LOGGER.info("STEP 5: ACTUAL : Seshat is not running on arm console");
		} else {
		    LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    }
	    LOGGER.info("**********************************************************************************");

	    step = "s6";
	    errorMessage = "Seshat binary is present in  Atom console  ";
	    result = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Verify seshat binary is not present using systemctl command in atom console");
	    LOGGER.info("STEP 6: ACTION : Execute systemctl list-units|grep -i seshat");
	    LOGGER.info("STEP 6: EXPECTED : Seshat binary should not be present in Atom console ");
	    LOGGER.info("**********************************************************************************");

	    if (isAtomSyncAvailable) {
		systemCtlCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.COMMAND_SYSTEMCTL, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.PROCESS_NAME_SESHAT);
		systemCtlResponse = CommonMethods.executeCommandInAtomConsole(device, tapEnv, systemCtlCommand);
		result = CommonMethods.isNull(systemCtlResponse);
		if (result) {
		    LOGGER.info("STEP 6: ACTUAL : Seshat binary is not present in Atom console");
		} else {
		    LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    } else {
		errorMessage = "ATOM console is not available for this device model";
		LOGGER.info("STEP 6: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");

	    step = "s7";
	    errorMessage = "Seshat binary is present in Arm console  ";
	    result = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION : Verify seshat binary is not present using systemctl command in arm console");
	    LOGGER.info("STEP 7: ACTION : Execute systemctl list-units|grep -i seshat");
	    LOGGER.info("STEP 7: EXPECTED : Seshat binary should not be present in Arm console ");
	    LOGGER.info("**********************************************************************************");

	    if (isAtomSyncAvailable) {
		errorMessage = "Not applicable for this device model";
		LOGGER.info("STEP 7: " + errorMessage);
		tapEnv.updateExecutionForAllStatus(device, testCaseId, step, ExecutionStatus.NOT_APPLICABLE,
			errorMessage, false);
	    } else {

		systemCtlCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandTestConstants.COMMAND_SYSTEMCTL, BroadBandTestConstants.SYMBOL_PIPE,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.PROCESS_NAME_SESHAT);
		systemCtlResponse = tapEnv.executeCommandUsingSsh(device, systemCtlCommand);
		result = CommonMethods.isNull(systemCtlResponse);

		if (result) {
		    LOGGER.info("STEP 7: ACTUAL : Seshat binary is not present in Arm console");
		} else {
		    LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    }

	    LOGGER.info("**********************************************************************************");

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(
		    "Exception occured while validating seshat service registration and component discovery distro - disabled scenario: "
			    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, false, errorMessage, true);
	}
    }
}
