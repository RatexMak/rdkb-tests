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

import org.apache.http.HttpStatus;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONObject;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.DeviceModeHandler;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpMib;
import com.automatics.rdkb.utils.snmp.BroadBandSnmpUtils;
import com.automatics.rdkb.utils.telemetry.BroadBandTelemetry2Utils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.webpa.WebPACommonUtils;
import com.automatics.snmp.SnmpDataType;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandMiscellaneousTests extends AutomaticsTestBase{
    
    
    /**
     * <ol>
     * <li>Verify whether webpa process is up</li>
     * <li>Create procanalyzerconfig.ini under nvram with dynamic values and verify file procanalyzerconfig.ini</li>
     * <li>Create processes.list under nvram anf verify /nvram/processes.list</li>
     * <li>Check the default value for the parameter "Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable" using webpa</li>
     * <li>Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent</li>
     * <li>Enable the parameter "Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable" using webpa</li>
     * <li>Verify the process is up for /usr/bin/cpuprocanalyzer</li>
     * <li>Verify the default values under /etc/procanalyzerconfig.ini is "FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60
     * FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1"</li>
     * <li>Check the captured data is saved under /tmp/cpuprocanalyzer</li>
     * <li>Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB</li>
     * <li>Verify CPUPROCANALYZERlog.txt.0 log is created</li>
     * <li>Veirfy console log on the log upload for the CPAstats</li>
     * <li>Verify the "/usr/bin/cpuprocanalyzer" exited after the log run.</li>
     * <li>Verify captured data in cpuprocanalyzer is not persistent on post reboot.</li>
     * 
     * @author Anuvarshini Manickavasagam Arulnambi
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-CPUPROC-1000")
    public void testToVerifyCpuprocanalyzer(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-CPUPROC-100";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CPUPROC-1000");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1.Verify whether webpa process is up");
	LOGGER.info(
		"2. Create procanalyzerconfig.ini under nvram with dynamic values  and verify file procanalyzerconfig.ini ");
	LOGGER.info("3. Create processes.list under nvram anf verify /nvram/processes.list");
	LOGGER.info(
		"4. Check the default value for the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	LOGGER.info("5. Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent");
	LOGGER.info("6. Enable the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	LOGGER.info("7. Verify the process is up for /usr/bin/cpuprocanalyzer");
	LOGGER.info(
		"8. Verify the default values under /etc/procanalyzerconfig.ini is FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	LOGGER.info("9. Check the captured data is saved under /tmp/cpuprocanalyzer");
	LOGGER.info("10. Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	LOGGER.info("11. Verify CPUPROCANALYZERlog.txt.0 log is created ");
	LOGGER.info("12. Verify console log on the log upload for the CPAstats");
	LOGGER.info("13. Verify the /usr/bin/cpuprocanalyzer exited after the log run.");
	LOGGER.info("14. Verify captured data in cpuprocanalyzer is not persistent on post reboot.");

	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Webpa process is not up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify whether webpa process is up");
	    LOGGER.info("STEP 1: ACTION : Execute command:pidof webpa");
	    LOGGER.info("STEP 1: EXPECTED : Webpa process should be up");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WebPA is Up and Running in the Device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("***********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "procanalyzerconfig.ini is not created under nvram ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION : Create procanalyzerconfig.ini under nvram with dynamic values  and verify file procanalyzerconfig.ini ");
	    LOGGER.info(
		    "STEP 2: ACTION : Copy procanalyzerconfig.ini from autovault\"Add below values:FEATURE.CPUPROCANALYZER.SLEEP.SECS = 30FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 90FEATURE.CPUPROCANALYZER.DYNAMIC = 1and save.Check cat /nvram/procanalyzerconfig.ini for saved values");
	    LOGGER.info("STEP 2: EXPECTED : File procanalyzerconfig.ini should get created under nvram.");
	    LOGGER.info("**********************************************************************************");

	    CommonUtils.downloadFileUsingAutoVault(device, tapEnv,
		    BroadbandPropertyFileHandler.getCpuProcAnalyzerFile(), BroadBandCommandConstants.MOUNT_NVRAM);
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_DYNAMIC,
		    BroadBandCommandConstants.FILE_PATH_CPU_PROC_NVRAM));

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : procanalyzerconfig.ini is created under nvram");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Processes not saved under /nvram/processes.list";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Create processes.list under nvram anf verify /nvram/processes.list");
	    LOGGER.info(
		    "STEP 3: ACTION : Copy processes.list from autovault\"Check if processes got saved in /nvram/processes.list Execute command:cat /nvram/processes.list");
	    LOGGER.info("STEP 3: EXPECTED : Processes should get saved in /nvram/processes.list");
	    LOGGER.info("**********************************************************************************");

	    CommonUtils.downloadFileUsingAutoVault(device, tapEnv, BroadbandPropertyFileHandler.getProcessListFile(),
		    BroadBandCommandConstants.MOUNT_NVRAM);
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandCommandConstants.CCSP_COMPONENT_PROCESS_NAME,
		    BroadBandCommandConstants.FILE_PATH_NVRAM_PROCESSES));

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Processes are saved under /nvram/processes.list");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"is not false";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Check the default value for the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute command:curl \"https://<url:port>/api/v2/device/mac:<MAC>/config?names=Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" -H \"AuThorization: Bearer <SAT token>\"");
	    LOGGER.info(
		    "STEP 4: EXPECTED : The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"should be false");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_CPU_PROC_ANALYZER, BroadBandTestConstants.FALSE);

	    if (status) {
		LOGGER.info(
			"STEP 4: ACTUAL :The default value of parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\"is retrieved as false");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "The value UPLOAD_LOGS_VAL_DCM is not set to true";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify UPLOAD_LOGS_VAL_DCM value is set to true using sysevent");
	    LOGGER.info("STEP 5: ACTION : Execute command:sysevent get UPLOAD_LOGS_VAL_DCM");
	    LOGGER.info("STEP 5: EXPECTED : UPLOAD_LOGS_VAL_DCM values should be true ");
	    LOGGER.info("***********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_SYSEVENT_GET, BroadBandTestConstants.VALUE_FOR_UPLOAD_LOGS_VAL_DCM));
	    LOGGER.info("The value is " + response);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
	    if (!status) {
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.SET_VALUE_FOR_UPLOAD_LOGS_VAL_DCM);
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SYSEVENT_GET,
				BroadBandTestConstants.VALUE_FOR_UPLOAD_LOGS_VAL_DCM));
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : UPLOAD_LOGS_VAL_DCM values is true");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Failed to enable the \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 6: DESCRIPTION : Enable the parameter \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" using webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Exeucte command:curl -X PATCH https://<url:port>/api/v2/device/mac:<MAC>/config -d \"{\"parameters\":[{\"name\":\"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\",\"value\":\"true\",\"dataType\":3}]}\" -H \"Content-Type:application/json\" -H \"Accept:application/json\" -H \"AuThorization: Bearer <SATTOKEN>\"");
	    LOGGER.info(
		    "STEP 6: EXPECTED : \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter value should get set to true with success message");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_CPU_PROC_ANALYZER, BroadBandTestConstants.CONSTANT_3,
		    BroadBandTestConstants.TRUE);

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Enabled the \"Device.SelfHeal.X_RDK_CPUProcAnalyzer_Enable\" parameter");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Process \"cpuprocanalyzer\" is not up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify the process is up for /usr/bin/cpuprocanalyzer");
	    LOGGER.info("STEP 7: ACTION : Execute command:pidof cpuprocanalyzer");
	    LOGGER.info("STEP 7: EXPECTED : The \"cpuprocanalyzer\" process should be up ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(CommonMethods.getPidOfProcess(device, tapEnv,
		    BroadBandCommandConstants.PROCESS_NAME_CPUPROCANALYZER));

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Process \"cpuprocanalyzer\" is up");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "The default values under /etc/procanalyzerconfig.ini  is not below: FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Verify the default values under /etc/procanalyzerconfig.ini is FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60  FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600  FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    LOGGER.info("STEP 8: ACTION : Execute command:cat /etc/procanalyzerconfig.ini");
	    LOGGER.info(
		    "STEP 8: EXPECTED : The default values under /etc/procanalyzerconfig.ini should be FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60  FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600  FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_SLEEP_SECS,
		    BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER)))
		;
	    errorMessage = "procanalyzerconfig.ini doesn’t contain FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 string";
	    {
		if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.FEATURE_CPUPROCANALYZER_TIMETORUN_SECS,
			BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER)))
		    ;
		errorMessage = "procanalyzerconfig.ini doesn’t contain FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 string";
		{
		    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTestConstants.FEATURE_CPUPROCANALYZER_DYNAMIC,
			    BroadBandCommandConstants.FILE_PATH_CPU_PROC_ANALYZER));
		}
	    }

	    if (status) {
		LOGGER.info(
			"STEP 8: ACTUAL : The default values under /etc/procanalyzerconfig.ini  is below: FEATURE.CPUPROCANALYZER.SLEEP.SECS = 60 FEATURE.CPUPROCANALYZER.TIMETORUN.SECS = 600 FEATURE.CPUPROCANALYZER.DYNAMIC = 1");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "The captured data folder cpuprocanalyzer is not under /tmp/cpuprocanalyzer";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Check the captured data folder cpuprocanalyzer is under /tmp/cpuprocanalyzer");
	    LOGGER.info("STEP 9: ACTION : Execute command:ls /tmp/cpuprocanalyzer");
	    LOGGER.info("STEP 9: EXPECTED : The captured data folder cpuprocanalyzer is under /tmp/cpuprocanalyzer");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.doesDirectoryExistInArmConsole(device, tapEnv,
		    BroadBandCommandConstants.FOLDER_PATH_CPU_PROC_ANALYZER,
		    BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS,
		    BroadBandTestConstants.TRUE).isStatus();

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : cpuprocanalyzer is available under tmp folder");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = " /tmp/cpuprocanalyzer folder is exceeding 1.5MB";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	    LOGGER.info("STEP 10: ACTION : Execute command:du -shc /tmp/* | grep -i \"cpuprocanalyzer\"");
	    LOGGER.info("STEP 10: EXPECTED :  /tmp/cpuprocanalyzer folder must not exceed 1.5MB");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.verifyDirectorySizeInArmConsole(device, tapEnv,
		    BroadBandCommandConstants.SIZE_OF_CPUPROCANALYZER, 1.5);

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : cpuprocanalyzer size is less than 1.5MB");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "\"Mem Limits curr :\" ,\"Triggering RunCPUProcAnalyzer.sh stop\" and \"Exiting the application\" log strings is not available in  CPUPROCANALYZERlog.txt.0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Verify CPUPROCANALYZERlog.txt.0 log is created ");
	    LOGGER.info("STEP 11: ACTION : Execute command:cat /rdklogs/logs/CPUPROCANALYZERlog.txt.0");
	    LOGGER.info(
		    "STEP 11: EXPECTED : \"Mem Limits curr :\" ,\"Triggering RunCPUProcAnalyzer.sh stop\" and \"Exiting the application\" log strings are available in CPUPROCANALYZERlog.txt.0");
	    LOGGER.info("***********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_TRIGGERING_RUNCPUPROCANALYZER,
		    BroadBandCommandConstants.COMMAND_CPUPROCANALYZER_LOG_FILE,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS))) {
		errorMessage = "CPUPROCANALYZERlog.txt.0 Log doesn’t contain the Triggering RunCPUProcAnalyzer.sh string";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.LOG_MESSAGE_EXITING_THE_APPLICATION,
			BroadBandCommandConstants.COMMAND_CPUPROCANALYZER_LOG_FILE,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL :CPUPROCANALYZERlog.txt.0 Log contains the required strings");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "Log not uploaded in the Console log.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Veirfy console log on the log upload for the CPAstats");
	    LOGGER.info(
		    "STEP 12: ACTION : Execute command:cat /rdklogs/logs/Consolelog.txt.0 For Atombased device:cat /rdklogs/logs/Armconsolelog.txt.0");
	    LOGGER.info("STEP 12: EXPECTED : The log upload should be uploaded in the Console ");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.isNotNull(BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_CPASTATS)))
		;
	    {
		errorMessage = "Log doesn’t contain the CPASTATS string";
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_LOGS_UPLOADED_SUCCESSFULLY));

	    }
	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Log uploaded in the Console log");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "Process \"cpuprocanalyzer\" is up";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify the process /usr/bin/cpuprocanalyzer exited after the log run.");
	    LOGGER.info("STEP 13: ACTION : Execute command:pidof cpuprocanalyzer");
	    LOGGER.info("STEP 13: EXPECTED : The \"cpuprocanalyzer\" process should not be up ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNull(CommonMethods.getPidOfProcess(device, tapEnv,
		    BroadBandCommandConstants.PROCESS_NAME_CPUPROCANALYZER));

	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Process \"cpuprocanalyzer\" is not up");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("*******************************************************8***************************");

	    stepNum = "s14";
	    errorMessage = "The  cpuprocanalyzer is present in the folder ";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 14: DESCRIPTION : Verify captured data in cpuprocanalyzer is not persistent on post reboot.");
	    LOGGER.info(
		    "STEP 14: ACTION : Execute command:Reboot.Device should come up and check the tmp folder after 10 mins of uptime.");
	    LOGGER.info("STEP 14: EXPECTED : The cpuprocanalyzer should not be present in the folder ");
	    LOGGER.info("**********************************************************************************");

	    if (CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)) {
		errorMessage = "Failed to verify CPUPROCANALYZER after reboot";
		status = !BroadBandCommonUtils.doesDirectoryExistInArmConsole(device, tapEnv,
			BroadBandCommandConstants.FOLDER_PATH_CPU_PROC_ANALYZER,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.TWENTY_SECOND_IN_MILLIS,
			BroadBandTestConstants.TRUE).isStatus();
	    }
	    if (status) {
		LOGGER.info("STEP 14: ACTUAL : The  cpuprocanalyzer is not present in the folder");
	    } else {
		LOGGER.error("STEP 14: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("***********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CPUPROC-1000");
    }
    
    /**
     * Device SHALL have the ability to retrieve the fan diagnostics via WebPA
     * <li>PRE-CONDITION-1 Get the number of fans installed in the device using TR-181 FanNumberOfEntries</li>
     * <li>PRE-CONDITION-2 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa</li>
     * <li>1. Verify the default value of Device.Thermal.Fan.{i}.Status using webpa</li>
     * <li>2. Verify the default value of Device.Thermal.Fan.{i}.Speed using webpa</li>
     * <li>3. Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa</li>
     * <li>4. Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa</li>
     * <li>5. Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa</li>
     * <li>6. Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa</li>
     * <li>7. Reboot the device</li>
     * <li>8. Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot</li>
     * <li>9. Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot</li>
     * <li>10. Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot</li>
     * <li>11. Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot</li>
     * <li>POST-CONDITION-1 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa</li>
     * 
     * @author Dipankar Nalui & Betel Costrow
     * @refactor Said Hisham
     *           </ol>
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-THERMAL_FAN-1000")
    public void testToVerifyFanParameters(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-THERMAL_FAN-100";
	String stepNum = null;
	String errorMessage = null;
	String response = null;
	boolean status = false;
	String fanSpeed = null;
	String noOfFanAvailable = null;

	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-THERMAL_FAN-1000");
	LOGGER.info("TEST DESCRIPTION: Device SHALL have the ability to retrieve the fan diagnostics via WebPA");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION-1 Get the number of fans installed in the device using TR-181 FanNumberOfEntries");
	LOGGER.info(
		"PRE-CONDITION-2 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	LOGGER.info("1. Verify the default value of Device.Thermal.Fan.{i}.Status using webpa");
	LOGGER.info("2. Verify the default value of Device.Thermal.Fan.{i}.Speed using webpa");
	LOGGER.info("3. Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa");
	LOGGER.info("4. Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	LOGGER.info("5. Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	LOGGER.info("6. Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	LOGGER.info("7. Reboot the device");
	LOGGER.info("8. Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot ");
	LOGGER.info("9. Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot ");
	LOGGER.info("10. Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot");
	LOGGER.info(
		"11. Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot ");
	LOGGER.info(
		"POST-CONDITION-1 Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa");

	LOGGER.info("#######################################################################################");

	try {

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION-1 : DESCRIPTION : Get the number of fans installed in the device using TR-181 FanNumberOfEntries");
	    LOGGER.info("PRE-CONDITION-1 : ACTION : Execute webpa get command: Device.Thermal.FanNumberOfEntries");
	    LOGGER.info(
		    "PRE-CONDITION-1 : EXPECTED : Device.Thermal.FanNumberOfEntries should provide 1 or 2 according to specific devices");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Unable to get no of fan is installed using Device.Thermal.FanNumberOfEntries";
	    noOfFanAvailable = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_NO_ENTRIES);
	    status = CommonMethods.isNotNull(noOfFanAvailable)
		    && !noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);

	    if (status) {
		LOGGER.info("PRE-CONDITION-1 : ACTUAL : Successfully got the number of fans available in device.");
	    } else {
		LOGGER.error("PRE-CONDITION-1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "PRE-CONDITION-2 : DESCRIPTION : Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	    LOGGER.info(
		    "PRE-CONDITION-2 : ACTION : Execute the following command:curl -H \\\"Authorization: Bearer <SAT_TOKEN> -X PATCH <WEBPA URL>device/mac:<MAC>/config -d \\\"{\\\"parameters\\\":[{\\\"dataType\\\":3,\\\"name\\\":\\\"Device.Thermal.Fan.{i}.MaxOverride\\\",\\\"value\\\":\\\"True\\\"}]}\\\"\");\r\n"
			    + "");
	    LOGGER.info(
		    "PRE-CONDITION-2 : EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to True is not successful";
	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("Waiting for 30 seconds to Fan configurations get updated.");

	    if (status) {
		LOGGER.info("PRE-CONDITION-2 : ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value is set successfully");
	    } else {
		LOGGER.error("PRE-CONDITION-2 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1: FAILED : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    stepNum = "s1";
	    errorMessage = "Device.Thermal.Fan.Status value is not retrieved as TRUE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.Status using webpa");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.Status");
	    LOGGER.info("STEP 1: EXPECTED : Device.Thermal.Fan.{i}.Status value should be retrieved as TRUE");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Device.Thermal.Fan.{i}.Status value is retrieved as TRUE");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "Device.Thermal.Fan.{i}.Speed value  is not  retrieved as an unsigned integer type.";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.Speed using webpa");
	    LOGGER.info(
		    "STEP 2: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.Speed");
	    LOGGER.info("STEP 2: EXPECTED : Device.Thermal.Fan.{i}.Speed value should be retrieved");
	    LOGGER.info("**********************************************************************************");

	    fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(fanSpeed)
		    && !fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(fanSpeed)
			&& !fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Device.Thermal.Fan.{i}.Speed value is retrieved");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "Device.Thermal.Fan.{i}.RotorLock value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Verify the default value of Device.Thermal.Fan.{i}.RotorLock using webpa");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/v2/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.RotorLock");
	    LOGGER.info("STEP 3: EXPECTED : Device.Thermal.Fan.{i}.RotorLock value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Device.Thermal.Fan.{i}.RotorLock value is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value  is not retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 4: DESCRIPTION : Verify the default value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 4: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value is not set successfully";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION : Set the value of Device.Thermal.Fan.{i}.MaxOverride to TRUE using webpa");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN> -X PATCH "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config -d \"{\"parameters\":[{\"dataType\":3,\"name\":\"Device.Thermal.Fan.{i}.MaxOverride\",\"value\":\"True\"}]}\"");
	    LOGGER.info("STEP 5: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.TRUE);
	    }

	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value is set successfully");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "Device.Thermal.Fan.{i}.MaxOverride value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.MaxOverride using webpa");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 6: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    errorMessage = "Failed to reboot and access the device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP 7: ACTION : Execute the following command: /sbin/reboot");
	    LOGGER.info("STEP 7: EXPECTED : Successfully rebooted the device.");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL : Device rebooted successfully");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s8";
	    errorMessage = "Device.Thermal.Fan.{i}.Status value is not retrieved as False";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Verify Device.Thermal.Fan.{i}.Status is not persisted after reboot");
	    LOGGER.info("STEP 8: ACTION : Execute webpa get command: Device.Thermal.Fan.{i}.Status");
	    LOGGER.info("STEP 8: EXPECTED : Device.Thermal.Fan.{i}.Status value should be retrieved as False");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcliAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_STATUS.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Device.Thermal.Fan.{i}.Status value  is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Device.Thermal.Fan.{i}.Speed value is not retrieved as 0";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Verify Device.Thermal.Fan.{i}.Speed is not persisted after reboot");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute webpa get command: Execute webpa get command: Device.Thermal.Fan.{i}.Speed");
	    LOGGER.info("STEP 9: EXPECTED : Device.Thermal.Fan.{i}.Speed value should be retrieved as 0");
	    LOGGER.info("**********************************************************************************");

	    fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(fanSpeed) && fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		fanSpeed = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_SPEED.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(fanSpeed)
			&& fanSpeed.equalsIgnoreCase(BroadBandTestConstants.STRING_ZERO);
	    }

	    if (status) {
		LOGGER.info("STEP 9: ACTUAL : Device.Thermal.Fan.{i}.Speed value is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "Device.Thermal.Fan.{i}.RotorLock value is not retrieved as FALSE/Not_Applicable";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Verify Device.Thermal.Fan.{i}.RotorLock is not persisted after reboot");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute webpa get command: Execute webpa get command: Device.Thermal.Fan.{i}.RotorLock");
	    LOGGER.info(
		    "STEP 10: EXPECTED : Device.Thermal.Fan.{i}.RotorLock value should be retrieved as FALSE/Not_Applicable");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE)
		    || response.equalsIgnoreCase(BroadBandTestConstants.STRING_FAN_NOT_APPLICABLE_ROTOR);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_ROTORLOCK.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE)
			|| response.equalsIgnoreCase(BroadBandTestConstants.STRING_FAN_NOT_APPLICABLE_ROTOR);
	    }

	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Device.Thermal.Fan.{i}.RotorLock value is not persisted after reboot");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "Device.Thermal.Fan.MaxOverride Value  is not  retrieved as FALSE";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION : Verify the value of Device.Thermal.Fan.{i}.MaxOverride once the device comes online after reboot ");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k "
			    + "<WEBPA URL>/device/mac:<ECM_MAC>/config?names=Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info("STEP 11: EXPECTED : Device.Thermal.Fan.{i}.MaxOverride Value should be retrieved as FALSE");
	    LOGGER.info("**********************************************************************************");

	    response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1));
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		response = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2));
		status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Device.Thermal.Fan.{i}.MaxOverride value  is retrieved as FALSE");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	} finally {
	    status = false;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "POST-CONDITION-1: DESCRIPTION :Set the value of Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride to False using webpa");
	    LOGGER.info("POST-CONDITION-1: ACTION : Execute webpa set command : Device.Thermal.Fan.{i}.MaxOverride");
	    LOGGER.info(
		    "POST-CONDITION -1: EXPECTED : Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride value should be set successfully");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE
			    .replace(BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_1),
		    WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    if (status && noOfFanAvailable.equalsIgnoreCase(BroadBandTestConstants.STRING_CONSTANT_2)) {
		status = BroadBandWebPaUtils.setParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_THERMAL_FAN_MAXOVERRIDE.replace(
				BroadBandTestConstants.TR181_NODE_REF, BroadBandTestConstants.STRING_CONSTANT_2),
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE);
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION -1: Device.Thermal.Fan.{i}.MaxOverride value is set to FALSE successfully");
	    } else {
		LOGGER.error(
			"POST-CONDITION -1: Device.DeviceInfo.Thermal.Fan.{i}.MaxOverride value is not set successfully");
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-THERMAL_FAN-1000");
    }
    
    /**
    *
    * Test Case : Verify CPE on-boarding process logging in the already onboarded device
    *
    * <p>
    * STEPS:
    * </p>
    * <ol>
    * <li>PRE-CONDITION 1 : Verify device accessible.</li>
    * <li>Step 1 : Verify that "syscfg get unit_activated" value is 1</li>
    * <li>Step 2 : Verify that \"/etc/ONBOARD_LOGGING_ENABLE\" file is created</li>
    * <li>Step 3 : Verify that \"/nvram/.device_onboarded\" file is created</li>
    * <li>Step 4 : Verify that get for \"Device.DeviceInfo.X_RDKCENTRAL-COM_OnBoarding_State\" parameter is "OnBoarded"
    * </li>
    * <li>Step 5 : Verify "sysevent get snmp-onboard-reboot" is EMPTY</li>
    * <li>Step 6 : Trigger DOCSIS SNMP REBOOT</li>
    * <li>Step 7 : Once the device comes up, verify the LastRebootReason as Docsis_SNMP_Reboot</li>
    * <li>Step 8 : Verify that \"OnBoardingLog*.txt.0\" file is not created after reboot</li>
    * <li>Step 9 : Trigger DOCSIS SNMP REBOOT</li>
    * <li>Step 10 : Once the device comes up, verify the LastRebootReason as Docsis_SNMP_Reboot</li>
    * <li>Step 11 : Verify log is captured to Consolelog.txt.0/ArmConsolelog.txt.0
    * 
    * @param device
    *            {@link Dut}
    * 
    * @author Dipankar Nalui
    * @refactor Govardhan
    *
    */
   @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
   @TestDetails(testUID = "TC-RDKB-ONBOARDING-1000")
   public void testToVerifyOnboardingProcessLoggingInTheAlreadyOnboardedDevice(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-ONBOARDING-100";
	int stepNumber = 1;
	String step = "S" + stepNumber;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	JSONObject parameters = new JSONObject();
	JSONArray params = new JSONArray();
	JSONObject jsonData = new JSONObject();
	String onboardLogsCreatedTime=null;
	String rebootReason=null;
	// Variable Declaration Ends
	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-ONBOARDING-1000");
	    LOGGER.info("TEST DESCRIPTION: Verify CPE on-boarding process logging in the already onboarded device");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(" PRE CONDITION 1 : Verify device accessible.");
	    LOGGER.info("1. Verify that \"syscfg get unit_activated\" value is 1 ");
	    LOGGER.info("2. Verify that ONBOARD_LOGGING_ENABLE file is created");
	    LOGGER.info("3. Verify that device_onboarded file is created");
	    LOGGER.info(
		    "4. Verify that get for \"Device.DeviceInfo.X_RDKCENTRAL-COM_OnBoarding_State\" parameter is \"OnBoarded\"");
	    LOGGER.info("5. Verify \"sysevent get snmp-onboard-reboot\" is EMPTY");
	    LOGGER.info("6. Trigger DOCSIS SNMP REBOOT ");
	    LOGGER.info("7. Once the device comes up, verify the LastRebootReason as Docsis_SNMP_Reboot");
	    LOGGER.info("8. Verify that \"OnBoardingLog*.txt.0\" file is not created after reboot");
	    LOGGER.info("9. Trigger DOCSIS SNMP REBOOT ");
	    LOGGER.info("10. Once the device comes up, verify the LastRebootReason as Docsis_SNMP_Reboot");
	    LOGGER.info(
		    "11. Verify log is captured to Consolelog.txt.0/ArmConsolelog.txt.0  RDKB_REBOOT: Device is up after reboot");
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION : Verify device accessible
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : VERIFY DEVICE ACCESSIBLE.");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : SSH THE DEVICE.");
	    LOGGER.info("PRE-CONDITION 1 : EXPTECTED : DEVICE MUSH BE ACCESSIBLE.");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "UNABLE TO SSH THE DEVICE.";
	    long startTime = System.currentTimeMillis();
	    try {
		do {
		    status = CommonMethods.isSTBAccessible(device);
		} while (!status
			&& ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS)
			&& BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    } catch (Exception exception) {
		LOGGER.error("Exception occured in accessing the device " + exception.getMessage());
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 1 : ACTUAL : DEVICE IS ACCESSIBLE.");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL : " + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION : 1 FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### ENDING PRE-CONFIGURATIONS ###################");

	    /**
	     * Step 1: VERIFY THAT "SYSCFG GET UNIT_ACTIVATED" VALUE IS 1
	     */
	    step = "S" + stepNumber;
	    errorMessage = "response does not show " + BroadBandTestConstants.STRING_VALUE_ONE;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify that \"syscfg get unit_activated\" value is 1 ");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Execute the following command: syscfg get unit_activated");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : response should show "
		    + BroadBandTestConstants.STRING_VALUE_ONE);
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_FOR_SYSCFG_GET, BroadBandTestConstants.STRING_UNIT_ACTIVATED));
	    status = CommonMethods.isNotNull(response)
		    && CommonMethods.patternMatcher(response.trim(), BroadBandTestConstants.STRING_VALUE_ONE);
	    if (status) {
		LOGGER.info(
			"STEP " + stepNumber + ": ACTUAL : response shows " + BroadBandTestConstants.STRING_VALUE_ONE);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 2: VERIFY THAT ONBOARD_LOGGING_ENABLE FILE IS CREATED
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = BroadBandCommandConstants.FILE_ETC_ONBOARD_LOGGING_ENABLE + " File is not available";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify that "+BroadBandCommandConstants.FILE_ETC_ONBOARD_LOGGING_ENABLE+" file is created ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command: ls -al "+BroadBandCommandConstants.FILE_ETC_ONBOARD_LOGGING_ENABLE);
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : /etc/ONBOARD_LOGGING_ENABLE File should be available");
	    LOGGER.info("**********************************************************************************");
	    status = CommonUtils.isFileExists(device, tapEnv,
		    BroadBandCommandConstants.FILE_ETC_ONBOARD_LOGGING_ENABLE);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
			+ BroadBandCommandConstants.FILE_ETC_ONBOARD_LOGGING_ENABLE + " File is available");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 3: VERIFY THAT DEVICE_ONBOARDED FILE IS CREATED
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED + " File is not available";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : Verify that "+BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED+" file is created ");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : Execute the following command: ls -al "+BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED);
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : " + BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED
		    + "File should be available");
	    LOGGER.info("**********************************************************************************");

	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + BroadBandCommandConstants.FILE_NVRAM_DEVICE_ONBOARDED
			+ " File is available");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 4: VERIFY THAT GET FOR \"DEVICE.DEVICEINFO.X_RDKCENTRAL-COM_ONBOARDING_STATE\" PARAMETER IS "
	     * ONBOARDED"
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Value of " + BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_ONBOARDING_STATE
		    + " is not returned as " + BroadBandTestConstants.STRING_ONBOARDED;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify that get for \"Device.DeviceInfo.X_RDKCENTRAL-COM_OnBoarding_State\" parameter is \"OnBoarded\"  ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command: curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA_URL><ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_OnBoarding_State");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : Value of OnBoarding_State should be returned as OnBoarded ");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_ONBOARDING_STATE,
		    BroadBandTestConstants.STRING_ONBOARDED, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    boolean isOnboardLogsFileExist = CommonUtils.isFileExists(device, tapEnv,
		    BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG);
	    if(isOnboardLogsFileExist) {
		onboardLogsCreatedTime=tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_ONBOARD_LOGS_TIME);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Value of "
			+ BroadBandWebPaConstants.WEBPA_PARAM_FEATURE_ONBOARDING_STATE + " is returned as "
			+ BroadBandTestConstants.STRING_ONBOARDED);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 5: VERIFY "SYSEVENT GET SNMP-ONBOARD-REBOOT" IS EMPTY
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "sysevent get snmp-onboard-reboot did not return EMPTY";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify \"sysevent get snmp-onboard-reboot\" is EMPTY");
	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION : Execute the following command:sysevent get snmp-onboard-reboot");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : sysevent get snmp-onboard-reboot should return EMPTY");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.isNull(tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SYSEVENT_GET,
			    BroadBandTestConstants.STRING_SNMP_ONBOARD_REBOOT)));
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : sysevent get snmp-onboard-reboot returned EMPTY");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 6: TRIGGER DOCSIS SNMP REBOOT
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Trigger of DOCSIS SNMP REBOOT is failed";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Trigger DOCSIS SNMP REBOOT ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command to Trigger DOCSIS SNMP REBOOT using mib  1.3.6.1.2.1.69.1.1.3.0 i 1");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Trigger of DOCSIS SNMP REBOOT should success");
	    LOGGER.info("**********************************************************************************");
	    if (DeviceModeHandler.isFibreDevice(device)) {
		response = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE);
		if (CommonMethods.isNotNull(response)) {
		    status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			    BroadBandTestConstants.CONSTANT_14);
		}
	    } else {
		if (BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getTableIndex())) {
		    status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			    BroadBandTestConstants.CONSTANT_14);
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Trigger of DOCSIS SNMP REBOOT is success");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 7: ONCE THE DEVICE COMES UP, VERIFY THE LASTREBOOTREASON AS DOCSIS_SNMP_REBOOT
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Last reboot reason is not snmp reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Once the device comes up, verify the LastRebootReason as snmp reboot");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA_URL><ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Last reboot reason should be snmp reboot");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
		status = BroadBandCommonUtils.verifySnmpRebootReason(device, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Last reboot reason is snmp reboot");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 8: VERIFY THAT \"ONBOARDINGLOG*.TXT.0\" FILE IS NOT CREATED AFTER REBOOT
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG
		    + "File is created after reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify that "
		    + BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG
		    + " file is not created after reboot");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command:ls -al /nvram2/onboardlogs/OnBoardingLog*.txt.0");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : "
		    + BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG
		    + "File should not be created after reboot");
	    LOGGER.info("**********************************************************************************");
	    response=tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_ONBOARD_LOGS_TIME);
	    status = !CommonUtils.isFileExists(device, tapEnv,
		    BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG) || response.equalsIgnoreCase(onboardLogsCreatedTime);
	   
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : "
			+ BroadBandCommandConstants.FILE_NVRAM_ONBOARDLOGS_ONBOARDINGLOG
			+ " file is not created after reboot");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 9 : TRIGGER DOCSIS SNMP REBOOT
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Trigger of DOCSIS SNMP REBOOT is failed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Trigger DOCSIS SNMP REBOOT ");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command to Trigger DOCSIS SNMP REBOOT using mib  1.3.6.1.2.1.69.1.1.3.0 i 1");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : response should be SNMPv2-SMI::mib-2.69.1.1.3.0 = INTEGER: 1");
	    LOGGER.info("**********************************************************************************");
	    if (DeviceModeHandler.isFibreDevice(device)) {
		response = BroadBandSnmpUtils.snmpSetOnEcm(tapEnv, device,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE);
		if (CommonMethods.isNotNull(response)) {
		    status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			    BroadBandTestConstants.CONSTANT_14);
		}
	    } else {
		if (BroadBandSnmpUtils.executeSnmpSetCommand(tapEnv, device,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getOid(), SnmpDataType.INTEGER,
			BroadBandTestConstants.STRING_VALUE_ONE,
			BroadBandSnmpMib.ENABLE_DOCSIS_SNMP_REBOOT.getTableIndex())) {
		    errorMessage = "Device is not rebooted after triggering snmp reboot";
		    status = CommonMethods.isSTBRebooted(tapEnv, device, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS,
			    BroadBandTestConstants.CONSTANT_14);
		}
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Trigger of DOCSIS SNMP REBOOT is success");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 10 : ONCE THE DEVICE COMES UP, VERIFY THE LASTREBOOTREASON AS DOCSIS_SNMP_REBOOT
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Last reboot reason is not SNMP Reboot";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Once the device comes up, verify the LastRebootReason as SNMP Reboot");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command:curl -H \"Authorization: Bearer <SAT_TOKEN>\" -k <WEBPA_URL><ECM_MAC>/config?names=Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Last reboot reason should be SNMP Reboot");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.waitForEstbIpAcquisition(tapEnv, device)) {
		status = BroadBandCommonUtils.verifySnmpRebootReason(device, tapEnv);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Last reboot reason is SNMP Reboot");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /**
	     * Step 11 : VERIFY LOG IS CAPTURED TO CONSOLELOG.TXT.0/ARMCONSOLELOG.TXT.0
	     */
	    stepNumber++;
	    step = "S" + stepNumber;
	    errorMessage = "Consolelog.txt.0 or ArmConsolelog.txt.0 did not show "
		    + BroadBandTestConstants.RDKB_REBOOT_REASON_ARM_CONSOLE;
	    status = false;
	    startTime = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Verify log is captured to Consolelog.txt.0/ArmConsolelog.txt.0  "
		    + BroadBandTestConstants.RDKB_REBOOT_REASON_ARM_CONSOLE);
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute the following command:grep -i \"RDKB_REBOOT\" /rdklogs/logs/*");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Consolelog.txt.0 or ArmConsolelog.txt.0 should show "
		    + BroadBandTestConstants.RDKB_REBOOT_REASON_ARM_CONSOLE);
	    LOGGER.info("**********************************************************************************");
	    do {
		response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadBandTestConstants.STRING_RDKB_REBOOT);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response.trim().toLowerCase(),
				BroadBandTestConstants.RDKB_REBOOT_REASON_ARM_CONSOLE.toLowerCase());
	    } while (!status
		    && ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS)
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Consolelog.txt.0 or ArmConsolelog.txt.0 showed "
			+ BroadBandTestConstants.RDKB_REBOOT_REASON_ARM_CONSOLE);
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
	LOGGER.info("ENDING TEST CASE: TC-RDKB-ONBOARDING-1000");
   }
   
   /**
   *
   * Test Case : Test case to verify Running Telemetry 2.0 process as non-root
   *
   * <p>
   * STEPS:
   * </p>
   * <ol>
   * <li>PRE CONDITION 1 : Check telemetry process is running in the device, If not enable the telemetry & verify</li>
   * <li>PRE CONDITION 2 : Set SelfHeal trigger interval to 4 minutes</li>
   * <li>1. Verify nonroot support blocklist parameter value is null or No blocklisted process by default</li>
   * <li>2. Check Telemetry2_0 process run as non-root</li>
   * <li>3. Kill Telemetry2_0 in the device</li>
   * <li>4. Verify minidump is created for killed Telemetry2_0 process</li>
   * <li>5. Verify Telemetry2_0 is recreated with non-root after selfheal</li>
   * <li>6. Verify parodus is running as unprivilege mode in CapDebug.txt</li>
   * <li>7. Verify the default capabilities for Telemetry2.0 are defined in JSON config file.</li>
   * <li>8. Enable nonroot support blocklist feature using RFC</li>
   * <li>9. Verify nonroot support blocklist feature parameter is Telemetry2.0 using RFC</li>
   * <li>10. Check Telemetry2_0 process run as root</li>
   * <li>11. Kill Telemetry2_0 process in the device</li>
   * <li>12. Verify minidump is created for killed Telemetry2_0 process</li>
   * <li>13. Verify Telemetry2_0 process is recreated after selfheal</li>
   * <li>POST CONDITION 1 : Set Null value to Non-root Blocklist parameter through RFC</li>
   * <li>POST CONDITION 2 : delete RFC feature</li>
   * </ol>
   * 
   * @param device
   *            {@link Dut}
   * 
   * @author Leela Krishnama Naidu Andela & Betel Costrow
   * @refactor yamini.s
   *
   */
  @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
  @TestDetails(testUID = "TC-RDKB-NONROOT-T2-1000")
  public void testToVerifyT2AsNonroot(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-NONROOT-T2-100";
	String stepNum = "s1";
	boolean status = false;
	String response = null;
	String errorMessage = "Not able to validate Running Telemetry 2.0 process as non-root";
	String telemetryNonRootLog = null;
	boolean telemetryEnabledDefault = false;
	String successMsg = null;
	// Variable Declaration Ends

	try {

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-NONROOT-T2-1000");
	    LOGGER.info("TEST DESCRIPTION: Verify Running Telemetry 2.0 process as non-root Previllages");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info(
		    " PRE CONDITION 1 : Check telemetry process is running in the device, If not enable the telemetry & verify");
	    LOGGER.info(" PRE CONDITION 2 : Set SelfHeal trigger interval to 4 minutes");
	    LOGGER.info(
		    "1. Verify nonroot support blocklist parameter value is null or No blocklisted process by default ");
	    LOGGER.info("2. Check Telemetry2_0 process run as non-root");
	    LOGGER.info("3. Kill Telemetry2_0 in the device");
	    LOGGER.info("4. Verify minidump is created for killed Telemetry2_0 process ");
	    LOGGER.info("5. Verify Telemetry2_0 is recreated with non-root after selfheal");
	    LOGGER.info("6. Verify parodus is running as unprivilege mode in CapDebug.txt");
	    LOGGER.info("7. Verify the default capabilities for Telemetry2.0 are defined in JSON config file.");
	    LOGGER.info("8. Enable nonroot support blocklist feature using RFC");
	    LOGGER.info("9. Verify nonroot support blocklist feature parameter is telemetry2_0 using RFC ");
	    LOGGER.info("10. Check Telemetry2_0 process run as root");
	    LOGGER.info("11. Kill  Telemetry2_0 process in the device");
	    LOGGER.info("12. Verify minidump is created for killed Telemetry2_0 process ");
	    LOGGER.info("13. Verify Telemetry2_0 process is recreated after selfheal");
	    LOGGER.info("POST CONDITION 1  : Set Null value to Non-root Blocklist parameter through RFC ");
	    LOGGER.info("POST CONDITION 2  : delete RFC feature ");
	    LOGGER.info("#######################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info(
		    "PRE-CONDITION 1: DESCRIPTION: Check telemetry process is running in the device, If not enable the telemetry & verify");
	    LOGGER.info("PRE-CONDITION 1: ACTION : 1) pidof telemetry2_0 2)Enable telemetry through webpa");
	    LOGGER.info("PRE-CONDITION 1: EXPECTED : Telemetry process should be running in the device");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to get the t2.0 process running status";
	    telemetryEnabledDefault = BroadBandCommonUtils.verifyProcessRunningStatus(device, tapEnv, false,
		    BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
	    successMsg = "Telemetry is by default enabled in the device";
	    if (!telemetryEnabledDefault) {
		if (BroadBandTelemetry2Utils.setTelemetry2ConfigurationViaWebpa(device, tapEnv)) {
		    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv)
			    && BroadBandTelemetry2Utils.verifyTelemetry2ConfigurationViaWebpa(device, tapEnv);
		    successMsg = "Enabled the telemetry in the device and verified telemetry process is running";
		}
	    } else {
		status = telemetryEnabledDefault;
	    }

	    if (status) {
		LOGGER.info("PRE-CONDITION 1: ACTUAL : " + successMsg);
	    } else {
		LOGGER.error("PRE-CONDITION 1: ACTUAL : " + errorMessage);
		throw new TestException(errorMessage);
	    }

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2: DESCRIPTION: Set SelfHeal trigger interval to 4 minutes");
	    LOGGER.info(
		    "PRE-CONDITION 2: ACTION : Execute webpa Command: tr181.Device.SelfHeal.ResourceMonitor.X_RDKCENTRAL-COM_UsageComputeWindow");
	    LOGGER.info("PRE-CONDITION 2: EXPECTED : SelfHeal trigger interval must be set to 4 minutes");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "Unable to set Self Heal Trigger interval to 4 mins";
	    if (BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_AGGRESSIVE_SELFHEAL_INTERVAL, WebPaDataTypes.INTEGER.getValue(),
		    BroadBandTestConstants.STRING_VALUE_TWO)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.USAGE_COMPUTE_WINDOW, WebPaDataTypes.INTEGER.getValue(),
			BroadBandTestConstants.STRING_VALUE_FOUR);
	    }
	    if (status) {
		LOGGER.info("PRE-CONDITION 2: ACTUAL : SelfHeal trigger interval is set to 4 minutes");
	    } else {
		LOGGER.error("PRE-CONDITION 2: ACTUAL : " + errorMessage);
		throw new TestException(errorMessage);
	    }

	    LOGGER.info("##################################################################################");

	    stepNum = "s1";
	    errorMessage = "Default value of Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Blocklist is not null or No blocklisted process";

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 1: DESCRIPTION : Verify nonroot support blocklist parameter value is null or No blocklisted process by default ");
	    LOGGER.info(
		    "STEP 1: ACTION : Execute:Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Blocklist");
	    LOGGER.info("STEP 1: EXPECTED : Default value should be null or No blocklisted process ");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandWebPaUtils.verifyDefaultValueForNonRootSupportList(device, tapEnv);
	    if (status) {
		LOGGER.info(
			"STEP 1: ACTUAL : Sucessfully verified non-root support blocklist parameter as null or No blocklisted process by default");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "telemetry2_0 process not running as non-root";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Check telemetry2_0 process run as non-root");
	    LOGGER.info("STEP 2: ACTION : Execute:ps | grep -nri -E \"telemetry2_0\" | grep bin");
	    LOGGER.info("STEP 2: EXPECTED : Response should have non-root along with telemetry2_0  process id");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS
		    .replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0));
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.STRING_NON_ROOT);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Successfuly verified parodus process is running as non-root");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "telemetry2_0 process is not killed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Kill telemetry2_0 in the device");
	    LOGGER.info(
		    "STEP 3: ACTION : Execute:1.get telemetry2_0 process using pidof telemetry2_02.kill -1 <pidof telemetry2_0>");
	    LOGGER.info("STEP 3: EXPECTED : telemetry2_0 process should be killed");
	    LOGGER.info("**********************************************************************************");
	    CommonUtils.clearLogFile(tapEnv, device, BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB);
	    status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv,
		    BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Successfully verified telemetry2_0 process id ");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s4";
	    errorMessage = "minidump is not created after telemetry2_0 process killed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify minidump is created for killed telemetry2_0 process ");
	    LOGGER.info("STEP 4: ACTION : Execute:grep -I \"Response code: 200\" /rdklogs/logs/core_log.txt");
	    LOGGER.info("STEP 4: EXPECTED : Response should have 200 success message ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_RESPONSE_200, BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL :Verified minidump is created for killed telemetry2_0 process");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s5";
	    errorMessage = "telemetry2_0 process not running as non-root";
	    status = false;
	    long startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Verify telemetry2_0 is recreated with non-root after selfheal");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute:ps | grep -nri -E \"telemetry2_0\" | grep binwait for 7 to 20min to get new process id");
	    LOGGER.info("STEP 5: EXPECTED : Response should have non-root along with telemetry2_0 process id");
	    LOGGER.info("**********************************************************************************");
	    do {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS.replace(BroadBandTestConstants.STRING_REPLACE,
				BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0));
		status = CommonMethods.isNotNull(response) && CommonUtils
			.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_NON_ROOT);
		if (CommonMethods.isNull(telemetryNonRootLog)) {
		    telemetryNonRootLog = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			    BroadBandTraceConstants.LOG_MESSAGE_UNPRIVILEGE_MODE_T2,
			    BroadBandCommandConstants.CMD_TO_GET_CAPDEBUG, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		}
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL : Successfuly verified telemetry2_0 process is running as non-root");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s6";
	    errorMessage = "telemtry2_0 is not running in unprivilege mode";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Verify telemtry2_0 is running as unprivilege mode in CapDebug.txt  ");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute:grep -I \"Dropping root privilege of telemtry2_0: runs as unprivilege mode\"  /rdklogs/logs/CapDebug.txt");
	    LOGGER.info("STEP 6: EXPECTED : Response should have uprivilege mode");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(telemetryNonRootLog);
	    LOGGER.info("Response captured from CapDebug.txt file " + telemetryNonRootLog);
	    if (!status) {
		telemetryNonRootLog = tapEnv.searchAndFindLineWithMatchingStringFromStart(device,
			BroadBandTraceConstants.LOG_MESSAGE_UNPRIVILEGE_MODE_T2,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(telemetryNonRootLog);
	    }

	    if (status) {
		LOGGER.info("STEP 6: ACTUAL : Successfully verified telemetry2_0 running as unprivilage mode");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s7";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 7: DESCRIPTION :Validate Default capabilities and Privilege adjustments defined in JSON config file");
	    LOGGER.info(
		    "STEP 7: ACTION :Execute linux  Command:cat /etc/security/caps/process-capabilities.json| grep -I process name");
	    LOGGER.info(
		    "STEP 7: EXPECTED :All process with nonroot privilleges should be listed under process-capabilities.json");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to find the process in process-capabilities.json";
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommandConstants.CMD_GET_ALL_PROCESS_CAPABILITIES);
	    status = CommonMethods.isNotNull(response) && CommonMethods.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);

	    if (status) {
		LOGGER.info("STEP 7: ACTUAL :All the  processes are present in the process-capabilities.json");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s8";
	    errorMessage = "Nonroot support blocklist feature is not telemetry2_0 using RFC";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION : Enable nonroot support blocklist feature using RFC");
	    LOGGER.info(
		    "STEP 8: ACTION : Using any rest client post the below content to RFC mock server: https://<XCONF_URL>/featureControl/updateSettings"
			    + "{\"estbMacAddress\":\"<MAC_ADDRESS>\",\"features\":[{\"name\":\"nonroot_support\",\"effectiveImmediate\":true,\"enable\":true,"
			    + "\"configData\":{\"tr181.Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Blocklist\":\"telemetry2_0\"}}]}and perform an reboot");
	    LOGGER.info("STEP 8: EXPECTED : Nonroot support feature should be telemetry2_0 using RFC");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.CONFIGURABLE_NONROOT_BLOCKLIST_TELEMETRY, true);

	    if (status) {
		LOGGER.info("STEP 8: ACTUAL : Succesfully enabled non-root support blocklist feature using RFC");
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s9";
	    errorMessage = "Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Blocklist is not telemetry2_0 using RFC";
	    status = false;
	    startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 9: DESCRIPTION : Verify nonroot support blocklist feature parameter is telemetry2_0 using RFC ");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute:Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.NonRootSupport.Blocklist");
	    LOGGER.info("STEP 9: EXPECTED : Nonroot support blocklist parameter response should be telemetry2_0");
	    LOGGER.info("**********************************************************************************");
	    do {
		response = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.TR181_PARAM_NONROOT_SUPPORT_BLOCKLIST);
		LOGGER.info("response :" + response);
		status = CommonMethods.isNotNull(response)
			&& response.contains(BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL : Sucessfully verified non-root support blocklist parameter is telemetry2_0 using webpa/dmcli");
	    } else {
		LOGGER.error("STEP 9: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s10";
	    errorMessage = "telemtery2_0 process not running as root";
	    status = false;
	    startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Check telemtery2_0 process run as root");
	    LOGGER.info("STEP 10: ACTION : Execute:ps | grep -nri -E \"telemtery2_0\" | grep bin");
	    LOGGER.info("STEP 10: EXPECTED : Response should have root along with telemtery2_0 process id");
	    LOGGER.info("**********************************************************************************");
	    do {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS.replace(BroadBandTestConstants.STRING_REPLACE,
				BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0));
		status = CommonMethods.isNotNull(response) && CommonUtils
			.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_ROOT);
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.TEN_MINUTE_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 10: ACTUAL : Successfuly verified telemtery2_0  process is running as root");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s11";
	    errorMessage = "telemtery2_0 process is not killed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION : Kill telemtery2_0 process in the device");
	    LOGGER.info(
		    "STEP 11: ACTION : Execute:1.get telemtery2_0 process using pidof telemtery2_02.kill -1 <telemtery2_0>");
	    LOGGER.info("STEP 11: EXPECTED : telemtery2_0 process should be killed");
	    LOGGER.info("**********************************************************************************");
	    CommonUtils.clearLogFile(tapEnv, device, BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB);
	    status = BroadBandCommonUtils.killAndCheckProcess(device, tapEnv,
		    BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0);

	    if (status) {
		LOGGER.info("STEP 11: ACTUAL : Successfully verified telemetry2_0 process id ");
	    } else {
		LOGGER.error("STEP 11: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s12";
	    errorMessage = "minidump is not created after telemetry2_0 process killed";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 12: DESCRIPTION : Verify minidump is created for killed telemetry2_0 process ");
	    LOGGER.info("STEP 12: ACTION : Execute:grep -I \"Response code: 200\" /rdklogs/logs/core_log.txt");
	    LOGGER.info("STEP 12: EXPECTED : Response should have 200 success message ");
	    LOGGER.info("**********************************************************************************");

	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_RESPONSE_200, BroadBandTestConstants.LOG_FILE_FOR_CRASHES_RDKB,
		    BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info("STEP 12: ACTUAL : Verified minidump is created for killed telemetry2_0 process");
	    } else {
		LOGGER.error("STEP 12: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s13";
	    errorMessage = "telemetry2_0 process not running as root";
	    status = false;
	    startTimeStamp = System.currentTimeMillis();
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 13: DESCRIPTION : Verify telemetry2_0 process is recreated after selfheal");
	    LOGGER.info(
		    "STEP 13: ACTION : Execute:ps | grep -nri -E \"telemetry2_0\" | grep binwait for 7 to 20min to get new process id");
	    LOGGER.info("STEP 13: EXPECTED : Response should have root along with telemetry2_0 process id");
	    LOGGER.info("**********************************************************************************");
	    do {
		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_GET_PROCESS_DETAILS.replace(BroadBandTestConstants.STRING_REPLACE,
				BroadBandTestConstants.PROCESS_NAME_TELEMETRY_2_0));
		status = CommonMethods.isNotNull(response) && CommonUtils
			.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_ROOT);
	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP 13: ACTUAL : Successfuly verified telemetry2_0 process is running as root");
	    } else {
		LOGGER.error("STEP 13: ACTUAL : " + errorMessage);
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
	    LOGGER.info("POST-CONDITION STEPS");
	    status = false;
	    successMsg = null;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : Set Null value to Non-root Blocklist parameter through RFC");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : 1)Send following HTTP DELETE request using POSTMAN/rest client:: https://<XCONF_URL>/featureControl/clear?estbMacAddress=<mac>&featureName=nonroot_support2) reboot the device twice");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : RFC set should be successful");
	    LOGGER.info("**********************************************************************************");

	    try {
		status = BroadBandRfcFeatureControlUtils.rfcFeatureWithSingleRebootRetrievewNow(tapEnv, device,
			BroadBandTestConstants.CONFIGURABLE_NONROOT_BLOCKLIST_TELEMETRY, true);
	    } catch (Exception e) {
		LOGGER.info("Exception occured while " + e.getMessage());
	    }

	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : Successfully set null value to Non-root Blocklist parameter.");
	    } else {
		LOGGER.error(
			"POST-CONDITION 1 : ACTUAL : Unable to set null value to Non-root Blocklist parameter though RFC");
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("POST-CONDITION 2 : DESCRIPTION : delete RFC feature");
	    LOGGER.info(
		    "POST-CONDITION 2 : ACTION : 1)Send following HTTP DELETE request using POSTMAN/rest client:: https://<XCONF_URL>/featureControl/clear?estbMacAddress=<mac>&featureName=nonroot_support2) reboot the device twice");
	    LOGGER.info("POST-CONDITION 2 : EXPECTED : RFC feature should be removed successfully");
	    LOGGER.info("**********************************************************************************");

	    status = (HttpStatus.SC_OK == BroadBandRfcFeatureControlUtils.clearSettingsInProxyXconfDcmServerForRDKB(
		    device, tapEnv, false, BroadBandTestConstants.CONFIGURABLE_NONROOT_BLOCKLIST_TELEMETRY));
	    if (status) {
		LOGGER.info("POST-CONDITION 2 : ACTUAL : Post condition executed successfully");
	    } else {
		LOGGER.error("POST-CONDITION 2 : ACTUAL : Post condition failed");
	    }
	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-NONROOT-T2-1000");
  }

  /**
   * Test to Verify that RFC requests contain accountId Unknown on Every powercycle
   * 
   * <li>PRE CONDITION 1. Reboot the device</li>
   * <li>1. Verify RFC xconf query has &accountID=unknown in the curl request</li>
   * <li>2. Check Account retrived from Dut by accountID RFC in /tmp/rfc_configdata.txt</li>
   * <li>3. Restart rfc-config.service to fetch the RFC settings from the mock xconf</li>
   * <li>4. Verify RFC xconf query &accountID=<accountID from Dut> in the curl request</li>
   * <li>5. Restart rfc-config.service to fetch the RFC settings from the mock xconf</li>
   * <li>6. Verify RFC xconf query &accountID=<accountID from Dut> in the subsquent xconf call</li>
   * <li>7. Remove the AccounID by setting invaild accountId</li>
   * <li>8. Verify Accountid is set in syscfg.db file using syscfg get command</li>
   * <li>9. Reboot the device</li>
   * <li>10. Verify RFC xconf query log for Account ID mismatch & accountID=unknown</li>
   * <li>11. Perform factory reset on the device</li>
   * <li>12.Verify RFC xconf query has &accountID=unknown in the curl request</li>
   *
   * @author Leela Krishnama Naidu Andela
   * @refactor Said hisham
   * 
   **/
  @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = TestGroup.SYSTEM)
  @TestDetails(testUID = "TC-RDKB-RFC-1202")
  public void testToVerifyAccountIdFromRfcRequest(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RFC-202";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	String response = null;
	String accountId = null;
	String accountIdFromSystem = null;
	String accountIdFromWebpa = null;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RFC-1202");
	LOGGER.info("TEST DESCRIPTION:Test to Verify that RFC requests contain accountId Unknown on Every powercycle");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1:Perform reboot on the device");
	LOGGER.info("1. Verify RFC xconf query has &accountID=unknown in the curl request");
	LOGGER.info("2. Check Account retrived from Dut by accountID RFC in  /tmp/rfc_configdata.txt ");
	LOGGER.info("3. Restart rfc-config.service to fetch the RFC settings from the mock xconf");
	LOGGER.info("4. Verify RFC xconf query &accountID=<accountID from Dut> in the curl request");
	LOGGER.info("5. Restart rfc-config.service to fetch the RFC settings from the mock xconf");
	LOGGER.info("6. Verify RFC xconf query &accountID=<accountID from Dut> in the subsquent xconf call");
	LOGGER.info("7. Remove the AccounID by setting  invaild accountId");
	LOGGER.info("8. Verify Accountid is set in syscfg.db file using syscfg get command");
	LOGGER.info("9. Reboot the device");
	LOGGER.info("10. Verify RFC xconf query log for Account ID mismatch & accountID=unknown");
	LOGGER.info("11. Perform factory reset on the device");
	LOGGER.info("12. Verify RFC xconf query has &accountID=unknown in the curl request");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    /**
	     * Pre-condition : perform reboot on the device
	     */
	    LOGGER.info("#######################################################################################");
	    // Reboot the Device.
	    LOGGER.info("PRE-CONDITION STEP 1: REBOOT THE DEVICE.");
	    LOGGER.info("PRE-CONDITION DESCRIPTION: REBOOT THE DEVICE USING COMMAND.");
	    LOGGER.info("PRE-CONDITION EXPTECTED: DEVICE MUST REBOOT AND COME UP.");
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    LOGGER.info("PRE-CONDITION ACTUAL : DEVICE REBOOTED SUCCESSFULLY.");
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");
	    /**
	     * Step 1 : Verify RFC xconf query has &accountID=unknown in the curl request
	     */
	    int stepNumber = BroadBandTestConstants.CONSTANT_1;
	    stepNum = "s" + stepNumber;
	    errorMessage = "accountID = Unknown is not being sent in DCM xconf request";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify xconf query has & accountID = Unknown in the curl request");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command:1) cat /rdklogs/logs/dcmrfc.log |grep -i Account ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The RFC curl request should have accountID = Unknown appended in it.");
	    LOGGER.info("**********************************************************************************");
	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, BroadBandCommandConstants.CMD_CURL,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_ACCOUNTID,
			    BroadBandTestConstants.STRING_UNKNOWN));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that RFC curl request  has accountID = Unknown in the curl request");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 2 : Check Account retrived from Dut by accountID RFC in /tmp/rfc_configdata.txt
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "tr181 parameter for accountId has not been stored in rfc_configdata.txt file";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " DESCRIPTION : Verify that the accountId value has been stored in persistent memory in the tr181 object Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.AccountInfo.AccountID");
	    LOGGER.info("STEP " + stepNumber
		    + " ACTION : Execute command:dmcli eRT getv Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.AccountInfo.AccountID");
	    LOGGER.info("STEP " + stepNumber
		    + " EXPECTED : /tmp/rfc_configdata.txt file should contain \"Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.AccountInfo.AccountID \" parameter and its value should be equal to accountId value");
	    LOGGER.info("**********************************************************************************");
	    accountId = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID);

	    BroadBandResultObject result = BroadBandRfcFeatureControlUtils.verifyParameterUpdatedByRfc(device, tapEnv,
		    BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID, accountId);
	    status = result.isStatus();

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " ACTUAL : AccountId has been found in tr181 Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.AccountInfo.AccountID");
	    } else {
		LOGGER.error("STEP " + stepNumber + " ACTUAL : " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    LOGGER.info("**********************************************************************************");

	    /**
	     * Step 3 : Restart rfc-config.service to fetch the RFC settings from the mock xconf
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to restart rfc service";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Restart rfc-config.service to fetch the RFC settings from the mock xconf");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : sh /lib/rdk/RFCbase.sh");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : rfc service should be restarted successfully");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.RFC_RESTART_SERVICE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.RFC_RESTART_SUCCEEDED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : rfc-config service has been restarted successfully");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 4 : Verify RFC xconf query has &accountID=<accountID> in the curl request
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "accountID = " + accountId + " is not being sent in DCM xconf request";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify xconf query has & accountID = " + accountId
		    + " in the curl request");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command:1) cat /rdklogs/logs/dcmrfc.log |grep -i Account ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The RFC curl request should have accountID = " + accountId
		    + " appended in it.");
	    LOGGER.info("**********************************************************************************");
	    if (CommonMethods.isAtomSyncAvailable(device, tapEnv)) {
		tapEnv.waitTill(BroadBandTestConstants.THREE_MINUTE_IN_MILLIS);
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.RFC_RESTART_SERVICE);
		status = CommonMethods.isNotNull(response) && CommonUtils
			.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.RFC_RESTART_SUCCEEDED);
	    }
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, BroadBandCommandConstants.CMD_CURL,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_ACCOUNTID, accountId));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that RFC curl request  has accountId as " + accountId
			+ " appended in it");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 5 : Restart rfc-config.service to fetch the RFC settings from the mock xconf
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Failed to restart rfc service";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Restart rfc-config.service to fetch the RFC settings from the mock xconf");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : sh /lib/rdk/RFCbase.sh");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : rfc service should be restarted successfully");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.RFC_RESTART_SERVICE);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.RFC_RESTART_SUCCEEDED);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : rfc-config service has been restarted successfully");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 6 : Verify RFC xconf query &accountID=<accountID from Dut> in the subsquent xconf call & http_code:
	     * 304
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "accountID = " + accountId + " is not being sent in DCM xconf request ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Verify xconf query has & accountID = " + accountId
		    + " http_code: 304 in the subsequent xconf call");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command:1) cat /rdklogs/logs/dcmrfc.log |grep -i Account ");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : The RFC curl request should have accountID = " + accountId
		    + " appended in it & http_code: 304");
	    LOGGER.info("**********************************************************************************");
	    tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, BroadBandCommandConstants.CMD_CURL,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_ACCOUNTID, accountId))) {
		errorMessage = " http_code: 304 not logged in /rdklogs/logs/dcmrfc.log file";
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.HTTP_RESPONSE_CODE_304, BroadBandTestConstants.DCMRFC_LOG_FILE,
			BroadBandTestConstants.TEN_MINUTE_IN_MILLIS, BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);

		status = CommonUtils.isNotEmptyOrNull(response) && CommonUtils
			.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.HTTP_RESPONSE_CODE_304);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that RFC curl request  has accountId as " + accountId
			+ " appended in it & http_code: 304");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 7 : Set Invalid AccountID with Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.AccountInfo.AccountID
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Unable to Set invalid AccoundID  via webpa/dmcli ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Set Invalid AccountID 1234567890 using web/dmcil");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute webpa command: "
		    + BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID);
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : WebPA Set operation should be successful ");
	    LOGGER.info("**********************************************************************************");
	    status = WebPACommonUtils.setWebPaParameter(tapEnv, device,
		    BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID, BroadBandTestConstants.INVAILD_ACCOUNT_ID,
		    BroadBandTestConstants.CONSTANT_0);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Invalid AccountId has been set via "
			+ BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID);
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 8: Verify Accountid is set in syscfg.db file using syscfg get command and cross validate via dmcli
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Unable to Verify Accountid is set in syscfg.db file using syscfg get command";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify Accountid is set in syscfg.db file using syscfg get command");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute command \"syscfg get AccountID\"");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : Successfully verified that AccountId is set in syscfg.db file");
	    LOGGER.info("**********************************************************************************");
	    accountIdFromSystem = tapEnv
		    .executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
			    BroadBandCommandConstants.CMD_FOR_SYSCFG_GET, BroadBandCommandConstants.CMD_FOR_ACCOUNTID))
		    .trim();
	    accountIdFromWebpa = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
		    BroadBandWebPaConstants.TR181_PARAM_TO_GET_ACCOUNT_ID);
	    status = CommonMethods.isNotNull(accountIdFromSystem) && CommonMethods.isNotNull(accountIdFromWebpa)
		    && CommonUtils.patternSearchFromTargetString(accountIdFromSystem, accountIdFromWebpa);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that AccountId is set in syscfg.db file");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	    /**
	     * Step 9 : Reboot the device
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Unable to perform reboot operation on the device";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + " : DESCRIPTION : Reboot the device");
	    LOGGER.info("STEP " + stepNumber + " : ACTION : Execute Command: /sbin/reboot");
	    LOGGER.info("STEP " + stepNumber + " : EXPECTED : Device should reboot and come up");
	    LOGGER.info("**********************************************************************************");
	    status = CommonMethods.rebootAndWaitForIpAccusition(device, tapEnv);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + " : ACTUAL : Successfully rebooted the device");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 10 : Verify RFC xconf query log for Account ID mismatch & accountID=unknown
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "accountID = Unknown is not being sent in DCM xconf request ";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify xconf query has & accountID = Unknown in the curl request & Account ID mismatch logged in /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command:1) cat /rdklogs/logs/dcmrfc.log |grep -i Account & Account Id mismatch: ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The RFC curl request should have accountID = Unknown appended in it & Account ID mismatch was logged in /rdklogs/logs/dcmrfc.log");
	    LOGGER.info("**********************************************************************************");
	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, BroadBandCommandConstants.CMD_CURL,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_ACCOUNTID,
			    BroadBandTestConstants.STRING_UNKNOWN))) {
		errorMessage = "Account ID mismatch is not logged in /rdklogs/logs/dcmrfc.log ";
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTestConstants.ACCOUNT_ID_MISMATCH, BroadBandTestConstants.DCMRFC_LOG_FILE,
			BroadBandTestConstants.SIXTEEN_MINUTES_IN_MILLIS,
			BroadBandTestConstants.FIVE_SECONDS_IN_MILLIS);

		status = CommonUtils.isGivenStringAvailableInCommandOutput(response,
			BroadBandTestConstants.ACCOUNT_ID_MISMATCH);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that RFC curl request  has accountID = Unknown & accountId mismatch");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * Step 11 : Factory reset the device
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "Unable to perform wifi factory reset operation on the device. hence blocking the execution.";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Do factory reset the device using webpa");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa set command: parameter: Device.X_CISCO_COM_DeviceControl.FactoryReset datatype: string value: Router,Wifi,VoIP,Dect,MoCA");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should be ssh able after factory reset");
	    LOGGER.info("**********************************************************************************");

	    status = BroadBandCommonUtils.performFactoryResetAndWaitForWebPaProcessToUp(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Devices came online after factory reset");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * Step 12 : Verify RFC xconf query has &accountID=unknown in the curl request
	     */
	    stepNumber++;
	    stepNum = "s" + stepNumber;
	    errorMessage = "accountID = Unknown is not being sent in DCM xconf request after Factory-reset";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify xconf query has & accountID = Unknown in the curl request");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command:1) cat /rdklogs/logs/dcmrfc.log |grep -i Account ");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : The RFC curl request should have accountID = Unknown appended in it.");
	    LOGGER.info("**********************************************************************************");
	    response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device, BroadBandCommandConstants.CMD_CURL,
		    BroadBandCommandConstants.FILE_DCMRFC_LOG, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    CommonMethods.concatStringUsingStringBuffer(BroadBandTestConstants.STRING_ACCOUNTID,
			    BroadBandTestConstants.STRING_UNKNOWN));
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ " : ACTUAL :Successfully verified that RFC curl request  has accountID = Unknown in the curl request after Factory reset");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);

	} finally {

	    BroadBandPostConditionUtils.executePostConditionToReActivateDevice(device, tapEnv, false,
		    BroadBandTestConstants.CONSTANT_1);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RFC-1202");

  }
}
