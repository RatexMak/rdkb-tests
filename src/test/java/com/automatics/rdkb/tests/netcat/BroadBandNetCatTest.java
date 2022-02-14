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

package com.automatics.rdkb.tests.netcat;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.BroadBandBandSteeringUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;

public class BroadBandNetCatTest extends AutomaticsTestBase {

    /**
     * This test is to verify that secure "nc" utility is implemented on Atom Console platforms to run only in client
     * mode.
     * <ol>
     * <li>Verify that "nc" utility is implemented on the device.</li>
     * <li>Verify that Netcat (\"nc\") utility does not support listening capability.</li>
     * <li>Verify that nc utility does not support execute capability.</li>
     * <li>Verify that nc does not support connection to any Private IP other than the range 172.31.255.x</li>
     * <li>Verify that nc cannot connect directly to any host outside of device.</li>
     * <li>Verify nc connection to the local host.</li>
     * 
     * @author Prince ArunRaj
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.SECURITY })
    @TestDetails(testUID = "TC-RDKB-NetCat-1003")
    public void netCatAtomTest(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-NetCat-103";
	String stepNum = "s1";
	String errorMessage = null;
	boolean status = false;
	String response = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-NetCat-1003");
	LOGGER.info(
		"TEST DESCRIPTION: This test is to verify that secure \"nc\" utility is implemented on Atom Console platforms to run only in client mode.");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify that \"nc\" utility is implemented on the device.");
	LOGGER.info("2. Verify that Netcat (\"nc\") utility does not support listening capability.");
	LOGGER.info("3. Verify that nc utility does not support execute capability.");
	LOGGER.info(
		"4. Verify that nc does not support connection to any Private IP other than the range 172.31.255.x");
	LOGGER.info("5. Verify that nc cannot connect directly to any host outside of device.");
	LOGGER.info("6. Verify nc connection to the local host.");

	LOGGER.info("#######################################################################################");

	try {

	    // Pre conditionS

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");

	    BroadBandPreConditionUtils.executePreconditionForBandSteering(tapEnv, device);

	    // Enable BandSteering

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION  : ENABLE BANDSTEERING");
	    LOGGER.info("PRE-CONDITION 2 : ACTION: EXECUTE WEBPA COMMAND");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED: BANDSTEERING SHOULD BE ENABLED");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO ENABLE BANDSTEERING !!!";
	    try {
		status = BroadBandBandSteeringUtils.enableDisableBandSteeringViaWebPa(device, tapEnv, true);
	    } catch (Exception e) {
		errorMessage = "EXCEPTION WHILE ENABLING BANDSTEERING." + e.getMessage();
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL : SUCCESSFULLY ENABLED BANDSTEERING");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL :" + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    stepNum = "s1";
	    errorMessage = "Netcat is not implemented on device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify that \"nc\" utility is implemented on the device.");
	    LOGGER.info("STEP 1: ACTION : Execute command in atomconsole:nc -v");
	    LOGGER.info("STEP 1: EXPECTED : Netcat busybox v1.22.1 should be implemented on the device.");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NETCAT_VERSION);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NETCAT_VERSION);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Netcat busybox is installed in the device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Invalid option -- \"I\" error is not found";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Verify that Netcat (\"nc\") utility does not support listening capability.");
	    LOGGER.info("STEP 2: ACTION : Execute command in atomconsole:nc -l 2222nc -nvlp 443");
	    LOGGER.info("STEP 2: EXPECTED : Both the commands should throw error \" invalid option -- \"l\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NC_LISTENING);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NC_INVALID_OPTION_L);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : commands throws error \" invalid option -- \"l\"");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "error \"invalid option -- \"e\" \" is not found";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify that nc utility does not support execute capability.");
	    LOGGER.info("STEP 3: ACTION : Execute command in atomconsole:nc -e /bin/sh");
	    LOGGER.info("STEP 3: EXPECTED : The command should throw error \"invalid option -- \"e\" \"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NC_EXECUTE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NC_INVALID_OPTION_E);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : The command throws error \"invalid option -- \"e\" ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = " error \"Bad Host\" is not found";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify that nc does not support connection to any Private IP other than the range 172.31.255.x");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command in atomconsole with any Private IP other than the range 172.31.255.x");
	    LOGGER.info("STEP 4: EXPECTED : All the commands should throw error \"Bad Host\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NC_BAD_HOST);
	    tapEnv.waitTill(30000);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NC_CONNECTION_ERROR_LAN_IP)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NC_CONNECTION_ERROR_IP_192)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NC_CONNECTION_ERROR_IP_169)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NC_CONNECTION_ERROR_IP_147);

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : All the commands throws error \"Bad Host\"");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + response + "\n error message : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "error \"Bad Host\" is not found";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify that nc cannot connect directly to any host outside of device.");
	    LOGGER.info("STEP 5: ACTION : Execute command in atomconsole:nc 10.0.0.23 7767");
	    LOGGER.info("STEP 5: EXPECTED : The command should throw error \"Bad Host\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NC_OUTSIDE_HOST);
	    tapEnv.waitTill(30000);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NC_CONNECTION_ERROR_IP_10);

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : The command throws error \"Bad Host\" 10.0.0.23");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Netcat console is not invoked";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify nc connection to the local host.");
	    LOGGER.info("STEP 6: ACTION : Execute command in atomconsole:nc 127.0.0.1 7787");
	    LOGGER.info(
		    "STEP 6: EXPECTED : Netcat should be invoked with below log.\"Netcat invoked with [tcp]mode Host[127.0.0.1] Port[7787]\"");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandOnAtom(device, BroadBandCommandConstants.CMD_NC_LOCAL_HOST);
	    tapEnv.waitTill(30000);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTestConstants.NC_NETCAT_INVOKED)
		    || CommonUtils.isGivenStringAvailableInCommandOutput(response,
			    BroadBandTraceConstants.LOG_MESSAGE_NC_NETCAT_INVOKED);

	    if (status) {
		LOGGER.info(
			"STEP 6: ACTUAL : Netcat invoked with below log.\"Netcat invoked with [tcp]mode Host[127.0.0.1] "
				+ "Use `dbg here' to see log messages; other dbg cmds for log level");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    BroadBandPostConditionUtils.executePostconditionForBandSteering(tapEnv, device);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-NetCat-1003");
    }
}